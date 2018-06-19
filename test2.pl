%==================================================%
%============== PREDICATS PRINCIPAUX ==============%
%==================================================%

% generer_random(Nombre, G).   -> Genere une grille avec un certain nombre d'element entré en parametre. Renvoi la grille dans G. La grille est valide et possède au moins une solution.

% resoud_sudoku(G, Sol).   -> Resoud la grille G et la place dans Sol. Appuyer sur ; permet d'avoir toutes les solutions. Echoue si la grille n'est pas valide ou ne possède plus de solutions.

%sudoku. -> Lance le menu.



%===================================================%
%========= DEFINITION DE GRILLES DE SUDOKU =========%
%===================================================%

% Exemple de grille de sudoku
sudoku(0,[[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0]]).
sudoku(1,[[1,2,0,5,6,7,9,7,2],[2,7,5,5,4,7,9,1,2],[3,7,6,5,4,7,5,1,3],[4,8,2,5,4,7,9,1,4],[5,9,2,5,4,7,9,1,5],[6,1,2,0,4,7,9,1,6],[7,6,2,5,4,7,9,1,7],[8,5,2,5,4,0,9,1,8],[9,7,3,5,4,7,9,1,9]]).
sudoku(2,[[1,0,0,0,0,0,0,0,0],[0,2,0,0,0,0,0,0,0],[0,0,3,0,0,0,0,0,0],[0,0,0,4,0,0,0,0,0],[0,0,0,0,5,0,0,0,0],[0,0,0,0,0,6,0,0,0],[0,0,0,0,0,0,7,0,0],[0,0,0,0,0,0,0,8,0],[0,0,0,0,0,0,0,0,9]]).
sudoku(3,[[4,1,5,6,3,8,9,7,2],[3,6,2,4,7,9,1,8,5],[7,8,9,2,1,5,3,6,4],[9,2,6,3,4,1,7,5,8],[1,3,8,7,5,6,4,2,9],[5,7,4,9,8,2,6,3,1],[2,5,7,1,6,4,8,9,3],[8,4,3,5,9,7,2,1,6],[6,9,1,8,2,3,5,4,7]]).
sudoku(4,[[0,0,0,0,0,0,0,0,0],[0,6,2,4,7,9,0,8,5],[7,0,9,2,0,5,3,6,4],[9,2,6,3,4,0,7,5,8],[1,3,8,0,5,0,4,2,9],[5,7,4,0,8,0,6,0,1],[2,5,0,1,6,4,0,0,3],[8,4,0,5,9,0,0,1,6],[6,0,1,8,0,3,5,4,7]]).
sudoku(5,[[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,4,6,0],[0,0,0,0,0,0,6,4,0]]).
sudoku(6,[[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0]]).

%====================================================%
%========= AFFICHAGE D'UNE GRILLE DE SUDOKU =========%
%====================================================%

% Afficher le sudoku 
afficher_grille(Grille) :- put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),nl,afficher_lignes(Grille), nl, nl.

% Afficher toutes les lignes
afficher_lignes([]).
afficher_lignes([Ligne1,Ligne2,Ligne3|Grille]) :- afficher_ligne(Ligne1),afficher_ligne(Ligne2),afficher_ligne(Ligne3),nl,put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),put(95),nl,afficher_lignes(Grille).

% Afficher une ligne particulière
afficher_ligne([Case1,Case2,Case3,Case4,Case5,Case6,Case7,Case8,Case9]) :- nl,put(124),put(32),afficher_case(Case1),put(32),afficher_case(Case2),put(32),afficher_case(Case3),put(32),put(124),put(32),afficher_case(Case4),put(32),afficher_case(Case5),put(32),afficher_case(Case6),put(32),put(124),put(32),afficher_case(Case7),put(32),afficher_case(Case8),put(32),afficher_case(Case9),put(32),put(124).

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
%retourner_colonne(Grille,Indice,Compteur,Liste,Res) :- element_i(Grille,Compteur,Ligne),element_i(Ligne,Indice,Valeur),append(Liste,Valeur,Res),C is Compteur+1,retourner_colonne(Grille,Indice,9,Liste,Res).

% Retourner le i-ème carré (numérotation de gauche à droite, et de haut en bas)
couper_lignes(_,1,4,[]).
couper_lignes(Grille,1,Compteur,[Liste|L]) :- Compteur>0,Compteur<4,retourner_ligne(Grille,Compteur,Liste),C is Compteur+1,couper_lignes(Grille,1,C,L).
couper_lignes([_|G],Indice,Compteur,Liste) :- Indice>1,Compteur>0,Compteur<4,I is Indice-1,couper_lignes(G,I,Compteur,Liste).

couper_colonnes([],_,_,[]).
couper_colonnes([Liste|L],Indice,Compteur,[Resultat|R]) :- mod(Indice,3)=:=1,couper_lignes(Liste,1,Compteur,Resultat),couper_colonnes(L,Indice,Compteur,R). % Carrés 1,4 et 7
couper_colonnes([Liste|L],Indice,Compteur,[Resultat|R]) :- mod(Indice,3)=:=2,couper_lignes(Liste,4,Compteur,Resultat),couper_colonnes(L,Indice,Compteur,R). % Carrés 2,5 et 8
couper_colonnes([Liste|L],Indice,Compteur,[Resultat|R]) :- mod(Indice,3)=:=0,couper_lignes(Liste,7,Compteur,Resultat),couper_colonnes(L,Indice,Compteur,R). % Carrés 3,6 et 9

retourner_carre([],_,[]) :- !.
retourner_carre(Grille,Indice,Resultat) :- Indice>0,Indice<4,couper_lignes(Grille,1,1,Liste),couper_colonnes(Liste,Indice,1,Resultat),!.
retourner_carre(Grille,Indice,Resultat) :- Indice>0,Indice<7,couper_lignes(Grille,4,1,Liste),couper_colonnes(Liste,Indice,1,Resultat),!.
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
%carre_valide([],4) :- !.
%carre_valide([Carre|Q],Compteur) :- Compteur<4,ligne_valide(Carre),C is Compteur+1,carre_valide(Q,C).

%Tester si un caree est valide
carre_valide(Carre) :- flat(Carre, Carref), ligne_valide(Carref).
%Applatir une liste
flat([], []).
flat([T|Q], Res) :- flat(T, TF), flat(Q,QF), concat(TF, QF, Res), !.
flat(T, [T]).

% Tester si une grille de sudoku est valide
sudoku_valide(Grille) :- grille_valide(Grille,1), !.

grille_valide(_,10) :- !.
grille_valide(Grille,Compteur) :- Compteur<10,retourner_ligne(Grille,Compteur,Ligne),ligne_valide(Ligne),
                                    retourner_colonne(Grille,Compteur,Colonne),colonne_valide(Colonne),
                                    retourner_carre(Grille,Compteur,Carre),carre_valide(Carre),C is Compteur+1,grille_valide(Grille,C).


%Verifie que la grille est valide en considerant qu une seule case a ete modifie (La case [Lig,Col]), permet de ne pas reverifier toute la grille a chaque mouvement
ajout_valide(Grille, [Col, Lig, _]) :- retourner_ligne(Grille,Lig,Ligne), ligne_valide(Ligne),retourner_colonne(Grille,Col,Colonne),colonne_valide(Colonne), Idcar is 3*((Lig-1)//3)+((Col-1)//3)+1,retourner_carre(Grille,Idcar,Carre),carre_valide(Carre), !.

% Vérifier que la coordonnée d'une case est valide
coordonnee_valide(X) :- X>0, X=<9, chiffre(X), !.
coordonnee_valide(_) :- nl, write('/!\ Coordonnee incorrecte'),nl,nl, fail.

% Tester si une grille de sudoku est pleine
grille_pleine([]).
grille_pleine([Liste|Reste]) :-­ \+ element_present(0,Liste), grille_pleine(Reste).

%=====================================================%
%==== GRILLES ATTEIGNALBLES EN AJOUTANT 1 CHIFFRE ====%
%=====================================================%

%Affiche successivement lemplacement (Colonne ResCol, position ResLig) des 0 de la grille. Echoue si il ny a aucun 0 dans la grille.
chercheZero(Grille,10,IndiceLig,ResCol,ResLig) :- ILig is IndiceLig +1, !,chercheZero(Grille, 1, ILig, ResCol, ResLig).
chercheZero(Grille,IndiceCol,IndiceLig,ResCol,ResLig) :- element_i(Grille, IndiceLig, Ligne), element_i(Ligne, IndiceCol, 0), ResCol is IndiceCol, ResLig is IndiceLig, !.
chercheZero(Grille,IndiceCol,IndiceLig,ResCol,ResLig) :- IndiceLig<10, ICol is IndiceCol +1, chercheZero(Grille, ICol, IndiceLig, ResCol, ResLig).


%Remplace une valeur dune liste par une autre
remplace([_|Q], 1, Valeur, [Valeur|Q]) :- !.
remplace([T|Q], Id, Valeur, [T|Res]) :- Id2 is Id-1, remplace(Q, Id2, Valeur, Res).

%Remplace une valeur dune grille par une autre
remplace_Grille(Grille, IdCol, IdLig, Valeur, Res) :- element_i(Grille, IdLig, Lig), remplace(Lig, IdCol, Valeur, NewLig), remplace(Grille, IdLig, NewLig, Res).

%Cherche un zero dans la grille et le remplace par une valeur entre 1 et 9 de sorte a ce que la grille obtenue soit valide
ajout_valeur(G,[ICol, Ilig, Val], NewG) :- chercheZero(G, 1, 1, ICol, Ilig),chiffre(Val), remplace_Grille(G, ICol, Ilig, Val, NewG), ajout_valide(NewG, [ICol, Ilig, Val]).

%Unifie X avec successivement tous les chiffres entre 1 et 9.
chiffre(X) :-element_present_no_cut(X, [1,2,3,4,5,6,7,8,9]).
element_present_no_cut(Element,[Element|_]).
element_present_no_cut(Element,[_|Liste]) :- element_present_no_cut(Element,Liste).

%Calcul le nombre de 0 dans une grille.
nombre_zero(_, 0, 10, 9) :- !.
nombre_zero(Grille, Nb, 10, L) :- NL is L+1, nombre_zero(Grille, Nb, 1, NL), !.
nombre_zero(Grille, Res, C, L) :- element_i(Grille, L, Ligne), element_i(Ligne, C, 0), NC is C+1, nombre_zero(Grille, Nb, NC, L), Res is Nb+1, !.
nombre_zero(Grille, Nb, C, L) :-  NC is C+1, nombre_zero(Grille, Nb, NC, L).

%=====================================================%
%========= RESOLUTION DE LA GRILLE DE SUDOKU =========%
%=====================================================%


%Resoud une grille de sudoku et affiche le resultat. Le nombre de 0 dans la grille est calculé au départ, et le predicat s'arrete quand il n'y a plus aucun 0 dans la grille, ou qu'aucun chiffre ne peut etre rajoute en gardant la grille valide.
resoud_sudoku(Grille, Sol) :- nombre_zero(Grille, Nb, 1, 1), solve(Grille, Nb, [], Sol).
solve(GrilleComp, 0, _, GrilleComp).
solve(GrillePartielle, Nb, Hist, GrilleRes):- ajout_valeur(GrillePartielle,Mvt, GrilleSucc), NewNb is Nb-1, solve(GrilleSucc, NewNb, [Mvt|Hist], GrilleRes).


%Afficher lensemble des etapes de la resolution dun sudoku a partir dun tableau de mouvement
afficher_etapes(G,[]) :- afficher_grille(G).
afficher_etapes(G,[Mvt|Mvts]) :- afficher_grille(G), ajout_valeur(G, Mvt, Ng), afficher_etapes(Ng, Mvts).


%===================================================%
%======== MODIFICATION D'UNE CASE DU SUDOKU ========%
%===================================================%

% Suppression d'une case
supprimer_valeur(Grille,IdColonne,IdLigne,G) :- remplace_Grille(Grille,IdColonne,IdLigne,0,G).

% Vérifier que la case est vide
case_vide(Grille,I,J) :- \+case_pleine(Grille,I,J),!.
case_vide(_,_,_) :- write('/!\ Cette case est pleine'),nl,fail.

% Vérifier que la case est pleine
case_pleine(Grille,I,J) :- retourner_ligne(Grille,I,Ligne),element_i(Ligne,J,Valeur),Valeur =\= 0,!.
case_pleine(_,_,_) :- write('/!\ Cette case est vide'),nl,fail.



%=====================================================%
%======== GENERATION D UNE GRILLE DE SUDOKU ==========%
%=====================================================%

%Cree une grille valide de Nb elements, ayant au moins une solution.
generer_random(Nb, Grille_incomplete) :- NewNb is 81-Nb, sudoku(0,G), sudoku_random(G,17,Grille_random17), resoud_sudoku(Grille_random17, Grille_Reso), effacer_chiffre(NewNb, Grille_Reso, Grille_incomplete), afficher_grille(Grille_incomplete), !.
sudoku_random(G, 0, G) :- !.
sudoku_random(G, Nb, NNG) :-  New_Nb is Nb-1, random(1,10,ICol), random(1,10,ILig) , element_i(G, ILig, Lig), element_i(Lig, ICol, 0),chiffre(Val), remplace_Grille(G, ICol, ILig, Val, NG), ajout_valide(NG, [ICol, ILig, Val]), sudoku_random(NG, New_Nb, NNG), !.
sudoku_random(G, Nb, NNG) :- sudoku_random(G, Nb, NNG).

%permet d'effacer un certain nombre de chiffres dans une grille en les remplacant par des 0. Efface seulement des cases qui ne sont pas deja des 0.
effacer_chiffre(0, G, G) :- !.
effacer_chiffre(Nb, G, Sol) :- random(1,10,ICol), random(1,10,ILig), element_i(G, ILig, Ligne), element_i(Ligne, ICol, Val), Val=\=0, remplace_Grille(G, ICol, ILig, 0, G_1zero), NewNb is Nb-1, effacer_chiffre(NewNb, G_1zero, Sol), !.
effacer_chiffre(Nb, G, Sol) :- effacer_chiffre(Nb, G, Sol).

%==================================================%
%============== ACTION SUR UN SUDOKU ==============%
%==================================================%

% Vérifier que le sudoku est terminé
resolution_valide(Grille,_) :- grille_pleine(Grille), write('!!! Sudoku termine !!!'),nl,!.
resolution_valide(_,Choix) :- Choix = 3.

% Verifier si la proposition de sudoku est terminée (choix 3 ou 4)
sudoku_entre(Choix) :- Choix = 3,!.
sudoku_entre(Choix) :- Choix = 4.


%==================================================%
%================= MENU PRINCIPAL =================%
%==================================================%

sudoku :- nl,
		write('%=============================%'),nl,
		write('%======= JEU DU SUDOKU =======%'),nl,
		write('%=============================%'),nl,nl,
			repeat, 
			menu, !.

menu :- write(' %=========== MENU ===========%'),nl,nl,
		write('1. Resoudre un sudoku de l\'ordinateur'),nl,
		write('2. Entrer un sudoku'),nl,
		write('3. Quitter'),nl,nl,
			read(Choix), nl,
			cas(Choix),
			Choix = 3, nl.

cas(1) :- write(' %===== RESOLUTION =====%'),nl,
			sudoku(4,Grille),
			asserta(resolution_sudoku(Grille)),
			repeat,
			resolution_manuelle, !.

cas(2) :- write(' %==== PROPOSITION ====%'),nl,
			sudoku(0,Grille),
			asserta(proposition_sudoku(Grille)),
			repeat,
			entrer_sudoku,!.

cas(3) :- write('%====== A bientot ======%'),!.
cas(_) :- write('/!\ Entrez 1,2 ou 3'),!.


%--------------------------------------------------%
%------------------- RESOLUTION -------------------%
%--------------------------------------------------%

% Menu résolution
resolution_manuelle :- nl,write('  %===== COMPLETEZ =====%'),nl,nl,
	resolution_sudoku(Grille), 
	afficher_grille(Grille), nl,
	write('1. Entrer un chiffre'), nl,
	write('2. Supprimer un chiffre'), nl,
	write('3. Quitter'), nl,
	write('4. Resoudre automatiquement'), nl, nl,
	read(Choix), nl,
	cas_resolution(Choix,Grille),
	resolution_sudoku(Grille_modifiee),
	resolution_valide(Grille_modifiee,Choix).

% Entrer un chiffre
cas_resolution(1,Grille) :- write('Ligne de la valeur a entrer : '),
	read(IdLigne),
	coordonnee_valide(IdLigne),nl,
	write('Colonne de la valeur a entrer : '),
	read(IdColonne),
	coordonnee_valide(IdColonne),nl,
	write('Valeur a entrer : '),
	read(Valeur),
	coordonnee_valide(Valeur),nl,
	case_vide(Grille,IdLigne,IdColonne),
	remplace_Grille(Grille,IdColonne,IdLigne,Valeur,G),
	sudoku_valide(G),
	afficher_grille(G), nl,
	retract(resolution_sudoku(Grille)),
	asserta(resolution_sudoku(G)),
	write('Valeur ajoutee, /!\ apres une suppression, il faut ajouter deux fois une valeur au meme emplacement'),nl,nl,!.

cas_resolution(1,_) :- !.

% Supprimer un chiffre
cas_resolution(2,Grille) :- write('Ligne de la valeur a supprimer : '),
	read(IdLigne),
	coordonnee_valide(IdLigne),nl,
	write('Colonne de la valeur a supprimer : '),
	read(IdColonne),
	coordonnee_valide(IdColonne),nl,
    case_pleine(Grille,IdLigne,IdColonne),
	supprimer_valeur(Grille,IdColonne,IdLigne,G),
    afficher_grille(G),nl,
	retract(resolution_sudoku(Grille)),
	asserta(resolution_sudoku(G)),
	write('Valeur supprimee'),nl,nl,!.

cas_resolution(2,_) :- !.

% Quitter
cas_resolution(3,_) :- !.

% Résolution automatique
cas_resolution(4,Grille) :- resoud_sudoku(Grille,Sol),afficher_grille(Sol),nl,nl,menu,!.

cas_resolution(4,_) :- !.

cas_resolution(_,_) :- nl, write('/!\ Entrez 1,2 ou 3'), nl, !, fail.


%---------------------------------------------------%
%------------------- PROPOSITION -------------------%
%---------------------------------------------------%

% Menu proposition
entrer_sudoku :- nl,write('  %=== ENTREZ LE SUDOKU ===%'),nl,nl,
	proposition_sudoku(Grille),
	afficher_grille(Grille), nl,
	write('1. Entrer un chiffre'), nl,
	write('2. Supprimer un chiffre'), nl,
	write('3. Quitter'), nl,
	write('4. Generer un sudoku automatiquement'), nl, nl,
	read(Choix), nl,
	cas_proposition(Choix,Grille),
	sudoku_entre(Choix).

% Entrer un chiffre
cas_proposition(1,Grille) :- write('Ligne de la valeur a entrer : '),
	read(IdLigne),
	coordonnee_valide(IdLigne),nl,
	write('Colonne de la valeur a entrer : '),
	read(IdColonne),
	coordonnee_valide(IdColonne),nl,
	write('Valeur a entrer : '),
	read(Valeur),
	coordonnee_valide(Valeur),nl,
	remplace_Grille(Grille,IdColonne,IdLigne,Valeur,G),
	sudoku_valide(G),
	afficher_grille(G), nl,
	asserta(proposition_sudoku(G)),
	retract(proposition_sudoku(Grille)),
	write('Valeur ajoutee, /!\ apres une suppression, il faut ajouter deux fois une valeur au meme emplacement'),nl,nl,!.

cas_proposition(1,_) :- !.

% Supprimer un chiffre
cas_proposition(2,Grille) :- write('Ligne de la valeur a supprimer : '),
	read(IdLigne),
	coordonnee_valide(IdLigne),nl,
	write('Colonne de la valeur a supprimer : '),
	read(IdColonne),
	coordonnee_valide(IdColonne),nl,
	supprimer_valeur(Grille,IdColonne,IdLigne,G),
	retract(proposition_sudoku(Grille)),
	asserta(proposition_sudoku(G)),
	write('Valeur supprimee'),nl,nl,!.

cas_proposition(2,_) :- !.

% Quitter
cas_proposition(3,_) :- !.

% Générer un sudoku aléatoire
cas_proposition(4,Grille) :- write('Nombre de chiffres voulus : '),
	read(Nb),nl,
    generer_random(Nb, Grille_random),nl,nl,
    write('1. Resoudre cette grille automatiquement'),nl,
    write('2. Retourner au menu'),nl,nl,
	read(ChoixRes), nl,
	cas_generation(ChoixRes,Grille_random),!.

cas_generation(1,Grille_random) :- resoud_sudoku(Grille_random,Sol),afficher_grille(Sol),nl,nl,!.
cas_generation(1,_) :- !.
cas_generation(2,Grille_random) :- nl,!.
cas_generation(2,_) :- !.
cas_generation(_,_) :- nl, write('/!\ Entrez 1 ou 2'), nl, !, fail.

cas_proposition(4,_) :- !.


cas_proposition(_,_) :- nl, write('/!\ Entrez 1,2,3 ou 4'), nl, !, fail.