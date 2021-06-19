% Base de conocimiento líneas del metro CDMX
% Moreno Zárate Víctor Gibran

% Color de cada línea...
%-------------------------------------------
color(línea_1, rosa).
color(línea_2, azul_marino).
color(línea_3, verde_olivo).
color(línea_4, azul_cielo).
color(línea_5, amarillo).
color(línea_6, rojo).
color(línea_7, naranja).
color(línea_8, verde_bandera).
color(línea_9, café).
color(línea_A, morado).
color(línea_B, gris).
color(línea_12, dorado).

% Trayecto de cada línea...
%-------------------------------------------
trayecto(línea_1, observatorio, pantitlán).
trayecto(línea_2, cuatro_caminos, tasqueña).
trayecto(línea_3, indios_verdes, universidad).
trayecto(línea_4, martín_carrera, santa_anita).
trayecto(línea_5, martín_carrera, santa_anita).
trayecto(línea_6, el_rosario, martín_carrera).
trayecto(línea_7, el_rosario, barranca_del_muerto).
trayecto(línea_8, garibaldi, constitución_de_1917).
trayecto(línea_9, pantitlán, tacubaya).
trayecto(línea_A, pantitlán, la_paz).
trayecto(línea_B, buenavista, ciudad_azteca).
trayecto(línea_12, mixcoac, tláhuac).

% Interestaciones...
%-------------------------------------------
sigue(pantitlán,zaragoza,línea_1).
sigue(zaragoza,gómez_farías,línea_1).
sigue(gómez_farías,boulevard_puerto_aéreo,línea_1).
sigue(boulevard_puerto_aéreo,balbuena,línea_1).
sigue(balbuena,moctezuma,línea_1).
sigue(moctezuma,san_lázaro,línea_1).
sigue(san_lázaro,candelaria,línea_1).
sigue(candelaria,merced,línea_1).
sigue(merced,pino_suárez,línea_1).
sigue(pino_suárez,isabel_la_católica,línea_1).
sigue(isabel_la_católica,salto_del_agua,línea_1).
sigue(salto_del_agua,balderas,línea_1).
sigue(balderas,cuauhtémoc,línea_1).
sigue(cuauhtémoc,insurgentes,línea_1).
sigue(insurgentes,sevilla,línea_1).
sigue(sevilla,chapultepec,línea_1).
sigue(chapultepec,juanacatlán,línea_1).
sigue(juanacatlán,tacubaya,línea_1).
sigue(tacubaya,observatorio,línea_1).
sigue(cuatro_caminos,panteones,línea_2).
sigue(panteones,tacuba,línea_2).
sigue(tacuba,cuitláhuac,línea_2).
sigue(cuitláhuac,popotla,línea_2).
sigue(popotla,colegio_militar,línea_2).
sigue(colegio_militar,normal,línea_2).
sigue(normal,san_cosme,línea_2).
sigue(san_cosme,revolución,línea_2).
sigue(revolución,hidalgo,línea_2).
sigue(hidalgo,bellas_artes,línea_2).
sigue(bellas_artes,allende,línea_2).
sigue(allende,zócalo,línea_2).
sigue(zócalo,pino_suárez,línea_2).
sigue(pino_suárez,san_antonio_abad,línea_2).
sigue(san_antonio_abad,chabacano,línea_2).
sigue(chabacano,viaducto,línea_2).
sigue(viaducto,xola,línea_2).
sigue(xola,villa_de_cortés,línea_2).
sigue(villa_de_cortés,nativitas,línea_2).
sigue(nativitas,portales,línea_2).
sigue(portales,ermita,línea_2).
sigue(ermita,general_anaya,línea_2).
sigue(general_anaya,tasqueña,línea_2).
sigue(indios_verdes,deportivo_18_de_marzo,línea_3).
sigue(deportivo_18_de_marzo,potrero,línea_3).
sigue(potrero,la_raza,línea_3).
sigue(la_raza,tlatelolco,línea_3).
sigue(tlatelolco,guerrero,línea_3).
sigue(guerrero,hidalgo,línea_3).
sigue(hidalgo,juárez,línea_3).
sigue(juárez,balderas,línea_3).
sigue(balderas,niños_héroes,línea_3).
sigue(niños_héroes,hospital_general,línea_3).
sigue(hospital_general,centro_médico,línea_3).
sigue(centro_médico,etiopía-plaza_de_la_transparencia,línea_3).
sigue(etiopía-plaza_de_la_transparencia,eugenia,línea_3).
sigue(eugenia,división_del_norte,línea_3).
sigue(división_del_norte,zapata,línea_3).
sigue(zapata,coyoacán,línea_3).
sigue(coyoacán,viveros-derechos_humanos,línea_3).
sigue(viveros-derechos_humanos,miguel_ángel_de_quevedo,línea_3).
sigue(miguel_ángel_de_quevedo,copilco,línea_3).
sigue(copilco,universidad,línea_3).
sigue(santa_anita,jamaica,línea_4).
sigue(jamaica,fray_servando,línea_4).
sigue(fray_servando,candelaria,línea_4).
sigue(candelaria,morelos,línea_4).
sigue(morelos,canal_del_norte,línea_4).
sigue(canal_del_norte,consulado,línea_4).
sigue(consulado,bondojito,línea_4).
sigue(bondojito,talismán,línea_4).
sigue(talismán,martín_carrera,línea_4).
sigue(politécnico,instituto_del_petróleo,línea_5).
sigue(instituto_del_petróleo,autobuses_del_norte,línea_5).
sigue(autobuses_del_norte,la_raza,línea_5).
sigue(la_raza,misterios,línea_5).
sigue(misterios,valle_gómez,línea_5).
sigue(valle_gómez,consulado,línea_5).
sigue(consulado,eduardo_molina,línea_5).
sigue(eduardo_molina,aragón,línea_5).
sigue(aragón,oceanía,línea_5).
sigue(oceanía,terminal_aérea,línea_5).
sigue(terminal_aérea,hangares,línea_5).
sigue(hangares,pantitlán,línea_5).
sigue(el_rosario,tezozómoc,línea_6).
sigue(tezozómoc,azcapotzalco,línea_6).
sigue(azcapotzalco,ferrería,línea_6).
sigue(ferrería,norte_45,línea_6).
sigue(norte_45,vallejo,línea_6).
sigue(vallejo,instituto_del_petróleo,línea_6).
sigue(instituto_del_petróleo,lindavista,línea_6).
sigue(lindavista,deportivo_18_de_marzo,línea_6).
sigue(deportivo_18_de_marzo,la_villa-basílica,línea_6).
sigue(la_villa-basílica,martín_carrera,línea_6).
sigue(el_rosario,aquíles_serdán,línea_7).
sigue(aquíles_serdán,camarones,línea_7).
sigue(camarones,refinería,línea_7).
sigue(refinería,tacuba,línea_7).
sigue(tacuba,san_joaquín,línea_7).
sigue(san_joaquín,polanco,línea_7).
sigue(polanco,auditorio,línea_7).
sigue(auditorio,constituyentes,línea_7).
sigue(constituyentes,tacubaya,línea_7).
sigue(tacubaya,san_pedro_de_los_pinos,línea_7).
sigue(san_pedro_de_los_pinos,san_antonio,línea_7).
sigue(san_antonio,mixcoac,línea_7).
sigue(mixcoac,barranca_del_muerto,línea_7).
sigue(garibaldi,bellas_artes,línea_8).
sigue(bellas_artes,san_juan_de_letrán,línea_8).
sigue(san_juan_de_letrán,salto_del_agua,línea_8).
sigue(salto_del_agua,doctores,línea_8).
sigue(doctores,obrera,línea_8).
sigue(obrera,chabacano,línea_8).
sigue(chabacano,la_viga,línea_8).
sigue(la_viga,santa_anita,línea_8).
sigue(santa_anita,coyuya,línea_8).
sigue(coyuya,iztacalco,línea_8).
sigue(iztacalco,apatlaco,línea_8).
sigue(apatlaco,aculco,línea_8).
sigue(aculco,escuadrón_201,línea_8).
sigue(escuadrón_201,atlalilco,línea_8).
sigue(atlalilco,iztapalapa,línea_8).
sigue(iztapalapa,cerro_de_la_estrella,línea_8).
sigue(cerro_de_la_estrella,uam_i,línea_8).
sigue(uam_i,constitución_de_1917,línea_8).
sigue(pantitlán,puebla,línea_9).
sigue(puebla,ciudad_deportiva,línea_9).
sigue(ciudad_deportiva,velódromo,línea_9).
sigue(velódromo,mixiuhca,línea_9).
sigue(mixiuhca,jamaica,línea_9).
sigue(jamaica,chabacano,línea_9).
sigue(chabacano,lázaro_cardenas,línea_9).
sigue(lázaro_cardenas,centro_médico,línea_9).
sigue(centro_médico,chilpancingo,línea_9).
sigue(chilpancingo,patriotismo,línea_9).
sigue(patriotismo,tacubaya,línea_9).
sigue(pantitlán,agrícola_oriental,línea_A).
sigue(agrícola_oriental,canal_de_san_juan,línea_A).
sigue(canal_de_san_juan,tepalcates,línea_A).
sigue(tepalcates,guelatao,línea_A).
sigue(guelatao,peñón_viejo,línea_A).
sigue(peñón_viejo,acatitla,línea_A).
sigue(acatitla,santa_marta,línea_A).
sigue(santa_marta,los_reyes,línea_A).
sigue(los_reyes,la_paz,línea_A).
sigue(ciudad_azteca,plaza_aragón,línea_B).
sigue(plaza_aragón,olímpica,línea_B).
sigue(olímpica,ecatepec,línea_B).
sigue(ecatepec,múzquiz,línea_B).
sigue(múzquiz,río_de_los_remedios,línea_B).
sigue(río_de_los_remedios,impulsora,línea_B).
sigue(impulsora,nezahualcóyotl,línea_B).
sigue(nezahualcóyotl,villa_de_aragón,línea_B).
sigue(villa_de_aragón,bosques_de_aragón,línea_B).
sigue(bosques_de_aragón,deportivo_oceanía,línea_B).
sigue(deportivo_oceanía,oceanía,línea_B).
sigue(oceanía,romero_rubio,línea_B).
sigue(romero_rubio,ricardo_flores_magón,línea_B).
sigue(ricardo_flores_magón,san_lázaro,línea_B).
sigue(san_lázaro,morelos,línea_B).
sigue(morelos,tepito,línea_B).
sigue(tepito,lagunilla,línea_B).
sigue(lagunilla,garibaldi,línea_B).
sigue(garibaldi,guerrero,línea_B).
sigue(guerrero,buenavista,línea_B).
sigue(tláhuac,tlaltenco,línea_12).
sigue(tlaltenco,zapotitlán,línea_12).
sigue(zapotitlán,nopalera,línea_12).
sigue(nopalera,olivos,línea_12).
sigue(olivos,tezonco,línea_12).
sigue(tezonco,periférico_oriente,línea_12).
sigue(periférico_oriente,calle_11,línea_12).
sigue(calle_11,lomas_estrella,línea_12).
sigue(lomas_estrella,san_andrés_tomatlán,línea_12).
sigue(san_andrés_tomatlán,culhuacán,línea_12).
sigue(culhuacán,atlalilco,línea_12).
sigue(atlalilco,mexicaltzingo,línea_12).
sigue(mexicaltzingo,ermita,línea_12).
sigue(ermita,eje_central,línea_12).
sigue(eje_central,parque_de_los_venados,línea_12).
sigue(parque_de_los_venados,zapata,línea_12).
sigue(zapata,hospital_20_de_noviembre,línea_12).
sigue(hospital_20_de_noviembre,insurgentes_sur,línea_12).
sigue(insurgentes_sur,mixcoac,línea_12).

% Reglas vistas en clase ...
%-------------------------------------------
conecta(X, Y, L) :- sigue(X, Y, L ) ; sigue(Y, X, L ). 
cerca(X, Y):- conecta(X, Y, _ ).
cerca(X, Y):- conecta(X, Z, _ ) , conecta(Z, Y, _ ).
alcanzable(X, Y):- conecta(X, Y, _).
alcanzable(X, Y):- conecta(X, Z, _ ), alcanzable(Z, Y).

% Predicado para consultar la lista de estaciones por donde se requiere pasar para  llegar de una estación 1 a otra estación 2
% USO: ruta(estacion1, estacion2, L).
% EJEMPLO: ruta(politécnico, buenavista, Camino).
%-------------------------------------------
ruta(N1,N2,Camino) :-  visita(N1,N2,[N1],Camino). 
visita(N1,N2,Resto,[N2|Resto]):-  conecta(N1,N2, _).
visita(N1,N2,YaVisitado,Camino):- conecta(N1,N3, _), N3 \== N2, not(member(N3,YaVisitado)),  visita(N3,N2,[N3|YaVisitado],Camino). 