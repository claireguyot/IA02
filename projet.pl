
%===================================================%
%========= DEFINITION DE GRILLES DE SUDOKU =========%
%===================================================%

% Exemple de grille de sudoku
sudoku(1,[[1,2,0,5,6,7,9,7,2],[2,7,5,5,4,7,9,1,2],[3,7,6,5,4,7,5,1,3],[4,8,2,5,4,7,9,1,4],[5,9,2,5,4,7,9,1,5],[6,1,2,0,4,7,9,1,6],[7,6,2,5,4,7,9,1,7],[8,5,2,5,4,0,9,1,8],[9,7,3,5,4,7,9,1,9]]).
sudoku(2,[[1,0,0,0,0,0,0,0,0],[0,2,0,0,0,0,0,0,0],[0,0,3,0,0,0,0,0,0],[0,0,0,4,0,0,0,0,0],[0,0,0,0,5,0,0,0,0],[0,0,0,0,0,6,0,0,0],[0,0,0,0,0,0,7,0,0],[0,0,0,0,0,0,0,8,0],[0,0,0,0,0,0,0,0,9]]).
sudoku(3,[[4,1,5,6,3,8,9,7,2],[3,6,2,4,7,9,1,8,5],[7,8,9,2,1,5,3,6,4],[9,2,6,3,4,1,7,5,8],[1,3,8,7,5,6,4,2,9],[5,7,4,9,8,2,6,3,1],[2,5,7,1,6,4,8,9,3],[8,4,3,5,9,7,2,1,6],[6,9,1,8,2,3,5,4,7]]).
sudoku(4,[[0,1,0,6,3,8,0,7,2],[0,6,2,0,7,9,1,8,0],[0,8,9,2,0,5,3,0,4],[9,0,6,0,4,0,7,5,8],[1,3,8,0,5,6,4,2,0],[5,7,0,9,8,2,0,0,1],[2,5,0,0,6,0,8,9,3],[8,4,3,0,9,7,0,1,6],[6,9,0,8,2,3,0,4,0]]).

%====================================================%
%========= AFFICHAGE D'UNE GRILLE DE SUDOKU =========%
%====================================================%

% Afficher le sudoku 
afficher_grille(Grille) :- write("  -----   -----   -----  "),afficher_lignes(Grille).

% Afficher toutes les lignes
afficher_lignes([]).
afficher_lignes([Ligne1,Ligne2,Ligne3|Grille]) :- afficher_ligne(Ligne1),afficher_ligne(Ligne2),afficher_ligne(Ligne3),nl,write("  -----   -----   -----  "),afficher_lignes(Grille).

% Afficher une ligne particulière
afficher_ligne([Case1,Case2,Case3,Case4,Case5,Case6,Case7,Case8,Case9]) :- nl,write("| "),afficher_case(Case1),write(" "),afficher_case(Case2),write(" "),afficher_case(Case3),write(" | "),afficher_case(Case4),write(" "),afficher_case(Case5),write(" "),afficher_case(Case6),write(" | "),afficher_case(Case7),write(" "),afficher_case(Case8),write(" "),afficher_case(Case9),write(" |").

% Afficher une case
afficher_case(0) :- put(32),!.
afficher_case(X) :- write(X),!.

%===============================================================%
%========= OBTENIR DES ELEMENTS DE LA GRILLE DE SUDOKU =========%
%===============================================================%

% Retourner le i-ème élément d'une liste
element_i([X|_],1,X) :- !.
element_i([_|Reste],Indice,Liste) :- I is Indice-1,element_i(Reste,I,Liste).

% Retourner la i-ème ligne (numérotation de haut en bas)
retourner_ligne(Grille,Indice,Liste) :- Indice>0,Indice<10,element_i(Grille,Indice,Liste).

% Concaténation de deux listes
concat([],Valeur,Valeur).
concat([T|X],Valeur,[T|Y]) :- concat(X,Valeur,Y).

% Retourner la i-ème colonne (numérotation de gauche à droite)
retourner_colonne([],_,[]).
retourner_colonne([Ligne|X],Indice,[Liste|Y]) :- Indice>0,Indice<10,element_i(Ligne,Indice,Liste),retourner_colonne(X,Indice,Y).

% Essai pour retourner_colonne :

%retourner_colonne(Grille,Indice,9,Liste,Res) :- element_i(Grille,9,Ligne), element_i(Ligne, Indice, Valeur), append(Liste,Valeur,Res),!.
%retourner_colonne(Grille,Indice,Compteur,Liste,Res) :- element_i(Grille,Compteur,Ligne),write(Compteur),element_i(Ligne,Indice,Valeur),append(Liste,Valeur,Res),C is Compteur+1,retourner_colonne(Grille,Indice,9,Liste,Res).

% Retourner le i-ème carré (numérotation de gauche à droite, et de haut en bas)
couper_lignes(_,1,4,[]).
couper_lignes(Grille,1,Compteur,[Liste|L]) :- Compteur>0,Compteur<4,retourner_ligne(Grille,Compteur,Liste),C is Compteur+1,couper_lignes(Grille,1,C,L).
couper_lignes([_|G],Indice,Compteur,Liste) :- Indice>1,Compteur>0,Compteur<4,I is Indice-1,couper_lignes(G,I,Compteur,Liste).

couper_colonnes([],_,_,[]).
couper_colonnes([Liste|L],Indice,Compteur,[Resultat|R]) :- mod(Indice,3)=:=1,couper_lignes(Liste,1,Compteur,Resultat),couper_colonnes(L,Indice,Compteur,R). % Carrés 1,4 et 7
couper_colonnes([Liste|L],Indice,Compteur,[Resultat|R]) :- mod(Indice,3)=:=2,couper_lignes(Liste,4,Compteur,Resultat),couper_colonnes(L,Indice,Compteur,R). % Carrés 2,5 et 8
couper_colonnes([Liste|L],Indice,Compteur,[Resultat|R]) :- mod(Indice,3)=:=0,couper_lignes(Liste,7,Compteur,Resultat),couper_colonnes(L,Indice,Compteur,R). % Carrés 3,6 et 9

retourner_carre([],_,[]) :- !.
retourner_carre(Grille,Indice,Resultat) :- Indice>0,Indice<4,couper_lignes(Grille,1,1,Liste),couper_colonnes(Liste,Indice,1,Resultat).
retourner_carre(Grille,Indice,Resultat) :- Indice>0,Indice<7,couper_lignes(Grille,4,1,Liste),couper_colonnes(Liste,Indice,1,Resultat).
retourner_carre(Grille,Indice,Resultat) :- Indice>0,couper_lignes(Grille,7,1,Liste),couper_colonnes(Liste,Indice,1,Resultat).

%===================================================%
%========= VALIDITE DE LA GRILLE DE SUDOKU =========%
%===================================================%

% Tester si un élément est présent dans une liste (p.117 du poly)
element_present(Element,[Element|_]) :- !.
element_present(Element,[_|Liste]) :- element_present(Element,Liste).

% Tester si une ligne est valide
ligne_valide([]).
ligne_valide([0|Reste]) :- !,ligne_valide(Reste).
ligne_valide([Case|Reste]) :- \+element_present(Case,Reste),ligne_valide(Reste).

% Tester si une colonne est valide
colonne_valide(Colonne) :- ligne_valide(Colonne).

% Tester si un carré est valide
carre_valide([],4) :- !.
carre_valide([Carre|Q],Compteur) :- Compteur<4,ligne_valide(Carre),C is Compteur+1,carre_valide(Q,C).

% Tester si une grille de sudoku est valide
sudoku_valide(Grille) :- grille_valide(Grille,1).

grille_valide(_,10) :- !.
grille_valide(Grille,Compteur) :- Compteur<10,retourner_ligne(Grille,Compteur,Ligne),ligne_valide(Ligne),
                                    retourner_colonne(Grille,Compteur,Colonne),colonne_valide(Colonne),
                                    retourner_carre(Grille,Compteur,Carre),carre_valide(Carre,1),C is Compteur+1,grille_valide(Grille,C).


%=====================================================%
%==== GRILLES ATTEIGNALBLES EN AJOUTANT 1 CHIFFRE ====%
%=====================================================%

%Affiche successivement lemplacement (Colonne ResCol, position ResVal) des 0 de la grille. Echoue si il ny a aucun 0 dans la grille.
chercheZero(Grille,IndiceCol,10,ResCol,ResVal) :- ICol is IndiceCol +1, !,chercheZero(Grille, ICol, 1, ResCol, ResVal).
chercheZero(Grille,IndiceCol,IndiceVal,ResCol,ResVal) :- element_i(Grille, IndiceCol, Col), element_i(Col, IndiceVal, 0), ResCol is IndiceCol, ResVal is IndiceVal.
chercheZero(Grille,IndiceCol,IndiceVal,ResCol,ResVal) :- IndiceCol<10, IVal is IndiceVal +1, chercheZero(Grille, IndiceCol, IVal, ResCol, ResVal).


%Remplace une valeur dune liste par une autre
remplace([_|Q], 1, Valeur, [Valeur|Q]).
remplace([T|Q], Id, Valeur, [T|Res]) :- Id2 is Id-1, remplace(Q, Id2, Valeur, Res).

%Remplace une valeur dune grille par une autre
remplace_Grille(Grille, IdCol, IdLigne, Valeur, Res) :- element_i(Grille, IdCol, Col), remplace(Col, IdLigne, Valeur, NewCol), remplace(Grille, IdCol, NewCol, Res).

ajout_valeur(G, [ICol, Ilig], NewG) :- chiffre(Val),chercheZero(G, 1, 1, ICol, Ilig), remplace_Grille(G, ICol, Ilig, Val, NewG).

chiffre(X) :-element_present_no_cut(X, [1,2,3,4,5,6,7,8,9]).
element_present_no_cut(Element,[Element|_]).
element_present_no_cut(Element,[_|Liste]) :- element_present_no_cut(Element,Liste).

%=====================================================%
%========= RESOLUTION DE LA GRILLE DE SUDOKU =========%
%=====================================================%



solve(GrilleComp, GrilleComp, _, []) :- \+chercheZero(GrilleComp, 1, 1, _, _), afficher_grille(GrilleComp).
solve(GrillePartielle, GrilleComp, History, [Mvt|Mvts]):- ajout_valeur(GrillePartielle, Mvt, GrilleSucc), sudoku_valide(GrilleSucc), solve(GrilleSucc, GrilleComp, [GrilleSucc|History], Mvts).




%=====================================================%
%======== GENERATION D'UNE GRILLE DE SUDOKU ==========%
%=====================================================%
