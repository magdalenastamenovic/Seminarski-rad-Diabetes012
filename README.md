# Seminarski-rad-Diabetes012-Health-Indicators-(BRFSS 2015)
Seminarskim radom analizirali smo skup podataka skup podataka Diabetes 012 – Health Indicators (BRFSS 2015) koji predstavlja veliki statistički skup zdravstvenih parametara prikupljenih u okviru istraživačkog programa u Sjedinjenim Američkim Državama. Nakon analize napravljen je model za predikciju promenljive Diabetes_012 (dijabetes), uz analizu faktora koji utiču na pojavu dijabetesa.

## Materijal
- [Koggle strana skupa podataka](https://www.kaggle.com/datasets/alexteboul/diabetes-health-indicators-dataset?resource=download): Link do sajta gde smo preuzeli skup podataka.  
- [diabetes_012_health_indicators_B.csv](https://www.kaggle.com/datasets/alexteboul/diabetes-health-indicators-dataset?resource=download&select=diabetes_012_health_indicators_BRFSS2015.csv): Fajl skupa podataka na sajtu.
- diabetes_dataset.csv: Preuzet skup podataka.
- README.md: Fajl na kome je napisan ovaj opis.  
- kod.R: R skripta sa kodom struktuirana po blokovima.
- Семинарски рад.docx: Word dokument sa opisima, graficima i tumačenjima istih.
- Семинарски рад.pdf: Pdf dokument sa opisima, graficima i tumačenjima istih.
- (biće naknadno dodat): R-markdown sa celokupnim izvršavanjem koda

## Skup Podataka
Ovaj skup podataka sadrži informacije o zdravstvenim navikama, prisustvu hroničnih bolesti, fizičkoj aktivnosti, ishrani, demografskim obeležjima i opštem zdravstvenom stanju.
Imamo sledeće kolone:

### Kategorijske promenljive
- **Diabetes_012**: Ispitanik ima stanje dijabetesa, predijabetesa ili nema dijabetes.  
- **HighBP**: Ispitanik ima visok krvni pritisak.  
- **HighChol**: Ispitanik ima povišen holesterol.  
- **CholCheck**: Ispitanik je proverio holesterol u poslednjih 5 godina.
- **Smoker**: Ispitanik je konzumirao 100 cigareta.
- **Stroke**: Ispitank je imao moždani udar.
- **HeartDiseaseorAttack**: Ispitanik je imao srčani udar ili ima hroničnu bolest srca.
- **PhysActivity**: Ispitanik je imao rekreativnu fizičku aktivnost poslednjih 30 dana.
- **Fruits**: Ispitanik konzumira voće svakog dana.
- **Veggies**: Ispitanik konzumira povrće svakog dana.
- **HvyAlcoholConsump**: Muškarac konzumira bar 14 pića nedeljno ili žena konzumira 7 pića nedeljno.
- **NoDocbcCost**: Ispitanik je imao potrebu poslednjih 12 meseci za doktorom i nije imao mogućnost da priušti.
- **GenHlth**: Ocena opšteg zdravlja.
- **DiffWalk**: Ispitanik ima potekoće u kretanju.
- **Sex**: Pol ispitanika.
- **Education**, **EducationCat**: Stepen edukacije.
- **Income**, **IncomeCat**: Godišnji prihod.
- **MentHlthCat**: Ocena stepena mentalnih problema.
- **PhysHlth**: Ocena stepena fizičkih problema.
- **AgeCat**: Starosna grupa.
- **CardioRiskScore**: Stepen kardiološkog rizika.
- **LifestyleRiskScore**: Stepen rizika životnog stila
- **HealthScore**: Stepen zdravlja.
- **DietScore**: Stepen ishrane.
- **SocioEconomicStatus**: Socijalno ekonomski status.


### Numeričke promenljive
- **BMI**: Indeks telesne mase.  
- **MentHlth**: Broj dana sa mentalnim poteškoćama ili stresom.
- **PhysHlth** Broj dana sa fizičkim poteškoćama.
- **Age** Starostna dob.

## Sadržaj rada
1.	Uvod
2.	Upoznavanje podataka: učitavanje i informisanje o skupu
3.	Priprema podataka za EDA
4.  Eksploratorna analiza podataka (EDA): univarijantna, bivarijantna analiza sa tabelama zaključaka
5.	Čišćenje podataka: Uklanjanje autlajera (ekstremnih vrednosti)
6.	Transformacija podataka: kategorizacija
7.	Inženjering karakteristika (Feature Engineering)	
8.	Bivarijantna analiza novih karakteristika	
9.	Selekcija prediktora		
10.	Modelovanje: trening i test skup, balansiranje podataka, regularizacija i PCA, treniranje modela (izbor, validacija, treniranje), metrike
11.  Reference

## Iskorišćeni modeli
- **Logistička Regresija**
- **Classification and Regression Trees (CART)**
- **Random Forest**
Metrike su pokazale da je Random FOrest naefikasniji od ova tri modela, prušajući najbolji balans izmedju preciznosti i osetljivosti.

## Zaključak
Analiza je pokazala da na pojavu bolesti dijabetesa ili predijabetičkog stanja najviše utiču gojaznost, srčane bolesti.
