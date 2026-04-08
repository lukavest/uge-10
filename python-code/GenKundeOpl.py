import random
from faker import Faker
fake = Faker()

def generate_random_cpr():
    """Generér et CPR-nummer i formatet xxxxxx-yyyy"""
    day = f"{random.randint(1, 28):02d}"  # Dag 01-28 (for simplicity, undgår månedslængdeproblemer)
    month = f"{random.randint(1, 12):02d}"  # Måned 01-12
    year = f"{random.randint(50, 99):02d}"  # Fødselsår (1950-1999)
    control_number = f"{random.randint(1000, 9999):04d}"  # Tilfældig 4-cifret kontrolnummer
    return f"{day}{month}{year}{control_number}"

def generate_kundeoplysninger(kundefil,kontofil, num_records):
    sider = ["th","tv"]

    with open(kundefil, "w") as f1, open(kontofil, "w") as f2:
        for _ in range(num_records):
            kunde_id = generate_random_cpr()
            fornavn = fake.first_name()
            efternavn = fake.last_name()

            konto_nummer = fake.bank_country()+fake.bban()
            #balance = random.random() * 9999999.0
            balance_int = int(random.randint(0,9999999) * random.random())
            balance_frac = random.randint(0,99)
            valuta_kode = fake.currency_code()

            vej = fake.street_name()
            husnr = fake.building_number()

            r = random.random()
            if r < 0.5:
                etage = ""
                side  = ""
            else:
                etage = random.randint(1,25)
                side = random.choice(sider)

            postnr = random.randint(1000,9999)
            land_kode = fake.country_code()

            telefon = random.randint(10000000,99999999)
            email = fake.email()
            
            by = fake.city()

            #print(f"{balance_int:>7d}"
            #    f"{balance_frac:02d}")

            # Sørg for, at alle felter står på faste positioner
            record1 = (
                f"{kunde_id:<10}"
                f"{fornavn:<20}"
                f"{efternavn:<20}"
                f"{konto_nummer:<20}"
                f"{balance_int:>7d}"
                f"{balance_frac:02d}"
                f"{valuta_kode:<3}"
                f"{vej:<30}"
                f"{husnr:<5}"
                f"{etage:<5}"
                f"{side:<5}"
                f"{by:<20}"
                f"{postnr:<4}"
                f"{land_kode:<2}"
                f"{telefon:<8}"
                f"{email:<50}"
                "\n"
            )
            f1.write(record1)
            
            accounts_owned = random.randint(0,3)
            
            for _ in range(accounts_owned):
            
                konto_id = random.randint(0,9999999999)
                # print(f"{konto_id:010d}")
                
                konto_typer = ["NemKonto", "Erhvervskonto", "Lønkonto", "Budgetkonto", "Opsparingskonto", "Pensionkonto", "Børneopsparing", "Studiekonto", "Fælleskonto"]
                konto_type = random.choice(konto_typer)
                
                record2 = (
                    f"{kunde_id:<10}"
                    f"{konto_id:010d}"
                    f"{konto_type:<20}"
                    f"{balance_int:>7d}"
                    f"{balance_frac:02d}"
                    f"{valuta_kode:<3}"
                    "\n"
                )
                f2.write(record2)
                balance_int = int(random.randint(0,9999999) * random.random())
                balance_frac = random.randint(0,99)

generate_kundeoplysninger("cobol-code/Kundeoplysninger.txt","cobol-code/KontoOpl.txt", 10)