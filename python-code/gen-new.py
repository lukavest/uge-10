import random
import datetime
import os

dir_path = os.path.dirname(os.path.realpath(__file__))

# Antal kunder i dataset
NUM_CUSTOMERS = 1000

# Maks antal transaktioner pr. kunde
MAX_TRANSACTIONS_PER_CUSTOMER = 10

# Antal banker
NUM_BANKS = 100

# Filnavne til outputfiler
TRANSACTION_FILE = os.path.join(dir_path, "../cobol-code/data/Transaktioner.txt")
# TRANSACTION_FILE =  "../cobol-code/data/Transaktioner.txt"
BANK_FILE = os.path.join(dir_path, "../cobol-code/data/Banker.txt")
# BANK_FILE = "../cobol-code/data/Banker.txt"

# Fiktive navne til generering
FIRST_NAMES = ["Lars", "Mette", "Jens", "Anne", "Peter", "Marie", "Søren", "Hanne", "Niels", "Camilla"]
LAST_NAMES = ["Hansen", "Jensen", "Nielsen", "Christensen", "Andersen", "Mortensen", "Larsen", "Pedersen", "Olsen", "Thomsen"]

# Fiktive adresser til kunder
STREETS = ["Østerbrogade", "Nørreport", "Amagerbrogade", "Vesterbrogade", "Hovedgaden", "Søndergade", "Strandvejen", "Frederiks Allé"]
CITIES = ["København", "Aarhus", "Odense", "Aalborg", "Esbjerg", "Randers", "Vejle", "Roskilde", "Helsingør", "Næstved"]
POSTCODES = ["2100", "8000", "5000", "9000", "6700", "8900", "7100", "4000", "3000", "4700"]

# Fiktive butikker
STORES = [
    "Supermarked", "Tøjbutik", "Elektronikbutik", "Restaurant", "Boghandel", 
    "Apotek", "Tankstation", "Café", "Biograf", "Møbelbutik", "Blomsterhandler", "Bageri", "Fitnesscenter"
]

# Fiktive banknavne
BANK_NAMES = ["Danske Bank", "Nordea", "Jyske Bank", "Sydbank", "Nykredit Bank", "Arbejdernes Landsbank", "Spar Nord Bank", "Handelsbanken"]

# Fiktive valutakoder og transaktionstyper
VALUTA_CODES = ["DKK", "USD", "EUR"]
TRANSACTION_TYPES = ["Indbetaling", "Udbetaling", "Overførsel"]

ENCODING =  "utf-8"  # Windows-1252 encoding for Danish characters

def pad_to_byte_length(value: str, byte_width: int, encoding: str = ENCODING, pad_char: str = " ") -> bytes:
    """Encode value and pad/truncate to exactly byte_width bytes."""
    encoded = value.encode(encoding)
    if len(encoded) > byte_width:
        # Truncate safely without splitting a multi-byte character
        encoded = encoded[:byte_width]
        # Walk back if we cut a mid-sequence byte (for safety)
        while len(encoded) > 0:
            try:
                encoded.decode(encoding)
                break
            except UnicodeDecodeError:
                encoded = encoded[:-1]
    pad_bytes = byte_width - len(encoded)
    return encoded + (pad_char.encode(encoding) * pad_bytes)


# Funktion til generering af en fødselsdato og CPR-lignende nummer
def generate_cpr():
    start_date = datetime.date(1950, 1, 1)
    end_date = datetime.date(2005, 12, 31)
    delta = end_date - start_date
    random_days = random.randint(0, delta.days)
    birth_date = start_date + datetime.timedelta(days=random_days)
    birth_date_str = birth_date.strftime("%d%m%y")  # Fødselsdato som DDMMYY
    random_suffix = f"{random.randint(1000, 9999):04}"  # CPR kontrolkode (4 cifre)
    return f"{birth_date_str}-{random_suffix}", birth_date.strftime("%d-%m-%Y")

# Funktion til generering af kontonummer
def generate_account_number():
    part1 = f"{random.randint(100, 999):03}"  # Første 3 cifre
    part2 = f"{random.randint(10, 99):02}"   # Næste 2 cifre
    part3 = f"{random.randint(10000, 99999):05}"  # Sidste 5 cifre
    return f"{part1}-{part2}-{part3}"  # Returnér kontonummeret

# Funktion til generering af adresse
def generate_address():
    street = random.choice(STREETS)
    house_number = f"{random.randint(1, 999)}{random.choice(['', 'A', 'B', 'C'])}"
    postcode = random.choice(POSTCODES)
    city = random.choice(CITIES)
    return f"{street} {house_number}, {postcode} {city}"

# Funktion til generering af transaktionsdato og tid med timestamp
def generate_transaction_timestamp():
    start_date = datetime.datetime(2020, 1, 1)
    end_date = datetime.datetime(2025, 12, 31, 23, 59, 59, 999999)
    delta = end_date - start_date
    random_seconds = random.randint(0, int(delta.total_seconds()))  # Konverter til int
    transaction_timestamp = start_date + datetime.timedelta(seconds=random_seconds)
    return transaction_timestamp.strftime("%Y-%m-%d-%H.%M.%S.%f")  # Timestamp i ønsket format # Timestamp i formatet YYYY-MM-DD-HH.MM.SS.MMMMMM

# Funktion til generering af bankoplysninger
def generate_bank_data():
    bank_data = []
    for i in range(1, NUM_BANKS + 1):
        reg_number = f"{i:04}"  # 4-cifret registreringsnummer
        bank_name = random.choice(BANK_NAMES)
        bank_address = generate_address()
        phone_number = f"+45 {random.randint(10000000, 99999999)}"
        email = f"kontakt@{bank_name.replace(' ', '').lower()}.dk"
        bank_data.append((reg_number, bank_name, bank_address, phone_number, email))
    return bank_data

def gen_data():
    # Generér bankfil
    bank_data = generate_bank_data()
    # with open(BANK_FILE, "w") as bank_file:
    with open(BANK_FILE, "w", encoding=ENCODING) as bank_file:
        for reg_number, bank_name, bank_address, phone_number, email in bank_data:
            bank_record = (
                f"{reg_number:<4}"         # Registreringsnummer (4 tegn)
                f"{bank_name:<30}"         # Banknavn (30 tegn)
                f"{bank_address:<50}"      # Bankadresse (50 tegn)
                f"{phone_number:<15}"      # Telefonnummer (15 tegn)
                f"{email:<40}"             # Emailadresse (*40 tegn)
            )
            bank_file.write(bank_record + "\n")

    # Generér transaktionsfil
    bank_registrations = [bank[0] for bank in bank_data]  # Liste med alle registreringsnumre fra bankfilen

    # with open(TRANSACTION_FILE, "w") as file:
    with open(TRANSACTION_FILE, "wb", encoding=ENCODING) as file:
        for i in range(1, NUM_CUSTOMERS + 1):  # Loop over kunder
            cpr, fødselsdato = generate_cpr()  # Generér CPR og fødselsdato
            konto_nummer = generate_account_number()  # Generér kontonummer
            reg_nummer = random.choice(bank_registrations)  # Vælg et tilfældigt registreringsnummer fra bankfilen
            navn = f"{random.choice(FIRST_NAMES)} {random.choice(LAST_NAMES)}"  # Fiktivt navn
            adresse = generate_address()  # Generér adresse

            recs_base = [(cpr, 15), (navn, 30), (adresse, 50), (fødselsdato, 11), (konto_nummer, 14), (reg_nummer, 6)]
            
            # Generér et tilfældigt antal transaktioner for denne kunde
            num_transactions = random.randint(1, MAX_TRANSACTIONS_PER_CUSTOMER)
            for _ in range(num_transactions):  # Loop over kundens transaktioner
                transaktions_beløb = round(random.uniform(-100000.00, 100000.00), 2)  # Tilfældig beløb
                # beløb_int = random.randint(-100000,100000)
                # beløb_frac = random.randint(0, 99)
                valutakode = random.choice(VALUTA_CODES)  # Tilfældig valutakode
                transaktions_type = random.choice(TRANSACTION_TYPES)  # Tilfældig type
                butik = random.choice(STORES)  # Tilfældig butik
                timestamp = generate_transaction_timestamp()  # Timestamp i formatet YYYY-MM-DD-HH.MM.SS.MMMMMM

                recs_full = recs_base + [(transaktions_beløb, 14), (valutakode, 4), (transaktions_type, 20), (butik, 20), (timestamp, 26)]
                
                record = ""
                for value, width in recs_full:
                    record += pad_to_byte_length(str(value), width)
                    
                file.write(record)
                
                # Formatér feltet til faste kolonner
                # record = (
                #     f"{cpr:<15}"             # Kundenummer (CPR-format)
                #     f"{navn:<30}"            # Navn
                #     f"{adresse:<50}"         # Adresse
                #     f"{fødselsdato:<11}"     # Fødselsdato (10 tegn + 1 mellemrum)
                #     f"{konto_nummer:<14}"    # Kontonummer (12 tegn + 2 mellemrum)
                #     f"{reg_nummer:<6}"       # Registreringsnummer
                #     f"{transaktions_beløb:>14.2f}"    # Højrestil beløb
                #     # f"{beløb_int:>11d}{beløb_frac:02}"   # Beløb som heltal + 2 decimaler
                #     f"{valutakode:<4}"       # Valutakode
                #     f"{transaktions_type:<20}"  # Transaktionstype
                #     f"{butik:<20}"           # Butik
                #     f"{timestamp:<26}"       # Timestamp
                # )
                # file.write(record + "\n")  # Skriv til fil og tilføj ny linje

    print(f"Dataset med {NUM_CUSTOMERS} kunder og op til {MAX_TRANSACTIONS_PER_CUSTOMER * NUM_CUSTOMERS} transaktioner er genereret i filen '{TRANSACTION_FILE}'")
    print(f"Bankdata genereret i filen '{BANK_FILE}'")
    
    
gen_data()

# TEST_FILE = os.path.join(dir_path, "../cobol-code/data/TST.txt")

# def tst_data():
#     with open(TEST_FILE, "wb") as file:
#         for i in range(10):
#             butik = STORES[i % len(STORES)]
#             ttype = TRANSACTION_TYPES[i % len(TRANSACTION_TYPES)]
#             record = pad_to_byte_length(butik, 20) + pad_to_byte_length(ttype, 20)  # Butik + Transaktionstype
#             # record = f"{butik:<20}"  # Butik
#             file.write(record)  # Skriv til fil og tilføj ny linje
            
            
# tst_data()