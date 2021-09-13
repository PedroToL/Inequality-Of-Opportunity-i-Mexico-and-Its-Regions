clear all

gl bases="F:\2021 Mexico\Article IOp\3 BASES DATOS ESRU-EMOVI 2017"

use "$bases\ESRU-EMOVI 2017 Entrevistado.dta", clear

****************************************
****************************************
***VARIABLES GENERALES
****************************************
****************************************




***REGION A LOS 14 AÑOS**************

*BC-Coahuila-Chihuahua-Monterrey-Sonora-Tamaulipas
gen region14=1 if p23==2| p23==5| p23==8| p23==19| p23==26| p23==28
*BCsur-Sinaloa-Zacatecas-Nayarit-Durango
replace region14=2 if p23==3| p23==10| p23==18| p23==25| p23==32
*Aguas-Colima-Jalisco-Michoacan-SLP
replace region14=3 if p23==1| p23==6| p23==14| p23==16| p23==24
*Guanajuato-Hidalgo-Mexico-Morelos-Puebla-Queretaro-Tlaxcala
replace region14=4 if p23==11| p23==13| p23==15| p23==17| p23==21| p23==22| p23==29
*CDMX
replace region14=5 if p23==9
*Campeche-Chiapas-Guerrero-Oaxaca-Qroo-Tabasco-Veracruz-Yucatan
replace region14=6 if p23==4| p23==7| p23==12| p23==20| p23==23| p23==27| p23==30| p23==31


*SOLO SE GUARDAN LOS QUE VIVIAN CON PAPA, MAMA O AMBOS
keep if p25==1 |  p25==2 | p25==3
*SOLO GUARDAMOS HOMBRES
keep if p06==2

*****************************************
*****************************************
**VARIABLES EDUCATIVAS
*****************************************
*****************************************

***Nivel educativo del entrevistado
gen educ=.
replace educ=1 if p13==97
replace educ=1 if p13==1

replace educ=2 if p13==2 & (p14>=1 & p14<=5)

replace educ=3 if p13==2 & p14==6
replace educ=3 if p13==3 & (p14>=1 & p14<=2)
replace educ=3 if p13==4 & (p14>=1 & p14<=2)

replace educ=4 if p13==3 & (p14>=3 & p14!=.)
replace educ=4 if p13==4 & (p14>=3 & p14!=.)
replace educ=4 if p13==5 & (p14>=1 & p14<=2)
replace educ=4 if p13==6 & (p14>=1 & p14<=2)
replace educ=4 if p13==7 & (p14>=1 & p14<=2)
replace educ=4 if p13==9 & (p14>=1 & p14<=2)

replace educ=5 if p13==5 & (p14>=3 & p14!=.)
replace educ=5 if p13==6 & (p14>=3 & p14!=.)
replace educ=5 if p13==7 & (p14>=3 & p14!=.)
replace educ=5 if p13==8 & (p14>=1 & p14<=2)
replace educ=5 if p13==9 & (p14>=3 & p14!=.)
replace educ=5 if p13==10 & (p14>=1 & p14<=3)
replace educ=5 if p13==11 & (p14>=1 & p14<=3)

replace educ=6 if p13==8 & (p14>=3 & p14!=.)
replace educ=6 if p13==10 & (p14>=4 & p14!=.)
replace educ=6 if p13==11 & (p14>=4 & p14!=.)
replace educ=6 if p13==12


***Nivel educativo del padre
gen educ_padre=.
replace educ_padre=1 if p42==2
replace educ_padre=1 if p43==1

replace educ_padre=2 if p43==2 & (p44>=1 & p44<=5)

replace educ_padre=3 if p43==2 & p44==6
replace educ_padre=3 if p43==3 & (p44>=1 & p44<=2)
replace educ_padre=3 if p43==4 & (p44>=1 & p44<=2)

replace educ_padre=4 if p43==3 & (p44>=3 & p44!=.)
replace educ_padre=4 if p43==4 & (p44>=3 & p44!=.)
replace educ_padre=4 if p43==5 & (p44>=1 & p44<=2)
replace educ_padre=4 if p43==6 & (p44>=1 & p44<=2)
replace educ_padre=4 if p43==7 & (p44>=1 & p44<=2)
replace educ_padre=4 if p43==9 & (p44>=1 & p44<=2)

replace educ_padre=5 if p43==5 & (p44>=3 & p44!=.)
replace educ_padre=5 if p43==6 & (p44>=3 & p44!=.)
replace educ_padre=5 if p43==7 & (p44>=3 & p44!=.)
replace educ_padre=5 if p43==8 & (p44>=1 & p44<=2)
replace educ_padre=5 if p43==9 & (p44>=3 & p44!=.)
replace educ_padre=5 if p43==10 & (p44>=1 & p44<=3)
replace educ_padre=5 if p43==11 & (p44>=1 & p44<=3)

replace educ_padre=6 if p43==8 & (p44>=3 & p44!=.)
replace educ_padre=6 if p43==10 & (p44>=4 & p44!=.)
replace educ_padre=6 if p43==11 & (p44>=4 & p44!=.)
replace educ_padre=6 if p43==12


***Nivel educativo de la madre
gen educ_madre=.

replace educ_madre=1 if p42m==2
replace educ_madre=1 if p43m==1

replace educ_madre=2 if p43m==2 & (p44m>=1 & p44m<=5)

replace educ_madre=3 if p43m==2 & p44m==6
replace educ_madre=3 if p43m==3 & (p44m>=1 & p44m<=2)
replace educ_madre=3 if p43m==4 & (p44m>=1 & p44m<=2)

replace educ_madre=4 if p43m==3 & (p44m>=3 & p44m!=.)
replace educ_madre=4 if p43m==4 & (p44m>=3 & p44m!=.)
replace educ_madre=4 if p43m==5 & (p44m>=1 & p44m<=2)
replace educ_madre=4 if p43m==6 & (p44m>=1 & p44m<=2)
replace educ_madre=4 if p43m==7 & (p44m>=1 & p44m<=2)
replace educ_madre=4 if p43m==9 & (p44m>=1 & p44m<=2)

replace educ_madre=5 if p43m==5 & (p44m>=3 & p44m!=.)
replace educ_madre=5 if p43m==6 & (p44m>=3 & p44m!=.)
replace educ_madre=5 if p43m==7 & (p44m>=3 & p44m!=.)
replace educ_madre=5 if p43m==8 & (p44m>=1 & p44m<=2)
replace educ_madre=5 if p43m==9 & (p44m>=3 & p44m!=.)
replace educ_madre=5 if p43m==10 & (p44m>=1 & p44m<=3)
replace educ_madre=5 if p43m==11 & (p44m>=1 & p44m<=3)

replace educ_madre=6 if p43m==8 & (p44m>=3 & p44m!=.)
replace educ_madre=6 if p43m==10 & (p44m>=4 & p44m!=.)
replace educ_madre=6 if p43m==11 & (p44m>=4 & p44m!=.)
replace educ_madre=6 if p43m==12


label var educ "Nivel educativo del entrevistado"
label var educ_padre "Nivel educativo del padre del entrevistado"
label var educ_madre "Nivel educativo de la madre del entrevistado"

label define educ	1 "Sin estudios" 					2 "Primaria incompleta" 					3 "Primaria" 					4 "Secundaria" 					5 "Preparatoria" 					6 "Profesional"
label value educ educ
label value educ_padre educ
label value educ_madre educ


****************************************
****************************************
***VARIABLES ÍNDICE SOCIOECONÓMICO
****************************************
****************************************

***Niveles educativos de los padres de los entrevistados
gen niveducp=1 if p42==2
replace niveducp=2 if p43==1
replace niveducp=3 if p43==2
replace niveducp=4 if p43==3 | p43==4
replace niveducp=5 if p43==5 | p43==6 | p43==7 | p43==9
replace niveducp=6 if p43==8 | p43==10 | p43==11 | p43==12


gen niveducm=1 if p42m==2
replace niveducm=2 if p43m==1
replace niveducm=3 if p43m==2
replace niveducm=4 if p43m==3 | p43m==4
replace niveducm=5 if p43m==5 | p43m==6 | p43m==7 | p43m==9
replace niveducm=6 if p43m==8 | p43m==10 | p43m==11 | p43m==12


gen niveduc=1 if p13==97
replace niveduc=2 if p13==1
replace niveduc=3 if p13==2
replace niveduc=4 if p13==3 | p13==4
replace niveduc=5 if p13==5 | p13==6 | p13==7 | p13==9
replace niveduc=6 if p13==8 | p13==10 | p13==11 | p13==12



***Años de educación del entrevistado y sus padres
gen  añosescp=.
replace  añosescp=0 if niveducp==1
replace  añosescp=p44 if niveducm==2
replace  añosescp=6+p44 if niveducp==3
replace  añosescp=9+p44 if niveducp==4
replace  añosescp=12+p44 if niveducp==5
replace  añosescp=16+p44 if niveducp==6
label var  añosescp "Años de escolaridad del padre"


gen añosescm=.
replace añosescm=0 if niveducm==1
replace añosescm=p44m if niveducm==2
replace añosescm=6+p44m if niveducm==3
replace añosescm=9+p44m if niveducm==4
replace añosescm=12+p44m if niveducm==5
replace añosescm=16+p44m if niveducm==6
label var añosescm "Años de escolaridad de la madre"


egen anesc_padres=rowmean( añosescp añosescm)
label var anesc_padres "Años de escolaridad promedio de los padres"


gen anesc_ent=.
replace anesc_ent=0 if niveduc==1
replace anesc_ent=p14 if niveduc==2
replace anesc_ent=6+p14 if niveduc==3
replace anesc_ent=9+p14 if niveduc==4
replace anesc_ent=12+p14 if niveduc==5
replace anesc_ent=16+p14 if niveduc==6
label var anesc_ent "Años de escolaridad del entrevistado"


****************************************
****************************************
***ÍNDICE DE RIQUEZA
****************************************
****************************************

***Hogar de origen
gen cuentaB=.
replace cuentaB=1 if p32_a==1 | p32_b==1
replace cuentaB=0 if p32_a==2 & p32_b==2

gen piso=.
replace piso=1 if p29==2 | p29==3
replace piso=0 if p29==1
label var piso "Piso de cemento o madera"

recode p30* p32* p33* p34* (2=0) (8=.)

pca  p30_a  p30_b p30_c p30_e  p33_a p33_b p33_e p33_d cuentaB     [aw=factor]

predict i_or if e(sample)
*replace i_or=i_or*(-1)
sum i_or[iw=factor]
gen iriq_or=(i_or-r(mean))/r(sd)
label var iriq_or "Índice de riqueza del hogar de origen"
xtile perctil_or=iriq_or[aw=factor], nq(100)
	 
***Hogar actual

recode p125* p126* p128* p129* (2=0)

pca p125b   p126a p126b p126c  p126e   p126j p126k p126l p126m  p126o p128c  p128d     [aw=factor]

predict i_des2 if e(sample)
*replace i_des2=i_des2*(-1)
sum i_des2 [aw=factor]
gen iriq_des2=(i_des2-r(mean))/r(sd)
label var iriq_des2 "Índice de riqueza del hogar de origen"
xtile perctil_des=iriq_des2 [aw=factor], nq(100)


sort folio

*Rural o Urbano a los  14 años
gen rural=0 if p24>0 & p24<5
replace rural=1 if  p24==5


 *Vivia con Padre, MAdre, Ambos aa los 14 años
sort p25


 **********
*Variables Dependientes
gen edad2=p05*p05


reg perctil_des p05 edad2
predict predict_porEdad
gen n_otcm_dest=perctil_des/predict_porEdad
xtile quintil_des=n_otcm_dest [aw=factor], nq(5)
xtile percentil_des=n_otcm_dest [aw=factor], nq(100)

reg perctil_or p05 edad2
predict predict_porEdad2
gen n_otcm_ori=perctil_or/predict_porEdad2
xtile quintil_or=n_otcm_ori [aw=factor], nq(5)
xtile percentil_or=n_otcm_ori [aw=factor], nq(100)
*==================================================================================*
*     ESCUELA PRIVADA O PUBLICA
*==================================================================================*
  
  gen priv_prim=0 if  p63a==3 |  p63a==4
  replace priv_prim=1 if  p63a==1 | p63a==2
  
  
  gen priv_secg=0 if p63b_1 ==3 | p63b_1 ==4
  replace priv_secg=1 if p63b_1 ==1  | p63b_1 ==2
    
  gen priv_sect=0 if p63b_2 ==3 | p63b_2 ==4
  replace priv_sect=1 if p63b_2 ==1  | p63b_2 ==2
  
  gen algun_priv=0 
  replace algun_priv=1 if priv_prim==1 | priv_secg==1 | priv_sect==1
  
  *==================================================================================*
*     CONDICIԎ DE HABLA IND·ENA DE LOS PADRES
*==================================================================================*
gen hli=0 if p39==2 | p39m==2
replace hli=1 if p39==1 | p39m==1

*Etiquetas
label var hli "Alguno de los padres habla alguna lengua indna" 
label define hli 0 "No habla lengua indna" 1 "Habla lengua indna" 
label values hli hli

*==================================================================================*
*Color de piel
*==================================================================================*

gen color_p=1 if p151==1 | p151==2 | p151==3
replace color_p=2 if p151==4 | p151==5
replace color_p=3 if p151==6| p151==7
replace color_p=4 if p151==8 | p151==9
replace color_p=5 if p151==10 | p151==11



 *Edad
 rename  p05 edad
 rename p06 sexo_inf
*contrato, prestacion
*dos bases una para los QUE TRABJAN Y ORTA PARA TODOS

*keep folio n_otcm_dest n_otcm_ori     factor    anesc_padres  edad sexo region14 rural 



tab quintil_or quintil_des [iw=factor]
tab quintil_or quintil_des [aw=factor]

 keep  folio sexo edad  percentil_or percentil_des  hli algun_priv rural region14    anesc_padres    factor     color_p  p151           
 order  folio sexo edad  percentil_or percentil_des  hli algun_priv rural region14    anesc_padres    factor     color_p  p151           


*Etiquetas

label var percentil_or "Percentil al origen" 
label var percentil_des "Percentil al destino" 
label var anesc_padres "Año escolaridad Padres (promedio)" 
label var sexo_inf "Sexo del Informante" 
label var algun_priv "Al menos una vez en una primaria o secundaria Privada" 
label var hli "Uno de los PAdres del Informante es Hablante de una lengua indigena" 
label var factor "Ponderador" 



save "C:\Users\DELL\Documents\IOP expost\2021\Septiembre\bases\Emovi_2017.dta", replace

