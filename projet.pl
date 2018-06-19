
%===================================================%
%========= DEFINITION DE GRILLES DE SUDOKU =========%
%===================================================%

% Exemple de grille de sudoku
sudoku(0,[[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0]]).
sudoku(1,[[1,2,0,5,6,7,9,7,2],[2,7,5,5,4,7,9,1,2],[3,7,6,5,4,7,5,1,3],[4,8,2,5,4,7,9,1,4],[5,9,2,5,4,7,9,1,5],[6,1,2,0,4,7,9,1,6],[7,6,2,5,4,7,9,1,7],[8,5,2,5,4,0,9,1,8],[9,7,3,5,4,7,9,1,9]]).
sudoku(2,[[1,0,0,0,0,0,0,0,0],[0,2,0,0,0,0,0,0,0],[0,0,3,0,0,0,0,0,0],[0,0,0,4,0,0,0,0,0],[0,0,0,0,5,0,0,0,0],[0,0,0,0,0,6,0,0,0],[0,0,0,0,0,0,7,0,0],[0,0,0,0,0,0,0,8,0],[0,0,0,0,0,0,0,0,9]]).
sudoku(3,[[4,1,5,6,3,8,9,7,2],[3,6,2,4,7,9,1,8,5],[7,8,9,2,1,5,3,6,4],[9,2,6,3,4,1,7,5,8],[1,3,8,7,5,6,4,2,9],[5,7,4,9,8,2,6,3,1],[2,5,7,1,6,4,8,9,3],[8,4,3,5,9,7,2,1,6],[6,9,1,8,2,3,5,4,7]]).
sudoku(4,[[0,1,0,6,3,8,0,7,2],[0,6,2,0,7,9,1,8,0],[0,8,9,2,0,5,3,0,4],[9,0,6,0,4,0,7,5,8],[1,3,8,0,5,6,4,2,0],[5,7,0,9,8,2,0,0,1],[2,5,0,0,6,0,8,9,3],[8,4,3,0,9,7,0,1,6],[6,9,0,8,2,3,0,4,0]]).
sudoku(5,[[0,1,0,2,0,0,0,0,0],[0,0,0,0,3,0,0,4,5],[0,0,0,0,6,0,7,0,0],[2,0,0,1,0,0,0,0,0],[5,0,0,0,0,0,0,0,6],[0,0,0,0,0,8,0,0,9],[0,0,7,0,2,0,0,0,0],[6,3,0,0,5,0,0,0,0],[0,0,0,0,0,4,0,8,0]]).



%====================================================%
%========= AFFICHAGE D'UNE GRILLE DE SUDOKU =========%
%====================================================%

% Afficher le sudoku 
afficher_grille(Grille) :- write("  -----   -----   -----  "),afficher_lignes(Grille),nl.

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

% Vérifier que X est un chiffre entre 1 et 9
chiffre(X) :- element_present_no_cut(X, [1,2,3,4,5,6,7,8,9]).

% Vérifier qu'il s'agit d'une liste
liste_valide([]).
liste_valide([_|B]):- liste_valide(B).

% Tester si un élément est présent dans une liste (p.117 du poly)
element_present(Element,[Element|_]) :- !.
element_present(Element,[_|Liste]) :- element_present(Element,Liste).

element_present_no_cut(Element,[Element|_]).
element_present_no_cut(Element,[_|Liste]) :- element_present_no_cut(Element,Liste).

% Tester si une ligne est valide
ligne_valide([]).
ligne_valide([0|Reste]) :- !,ligne_valide(Reste).
ligne_valide([Case|Reste]) :- \+element_present(Case,Reste),ligne_valide(Reste).

% Tester si une colonne est valide
colonne_valide(Colonne) :- ligne_valide(Colonne).

% Tester si un carré est valide
carre_valide([],4) :- !.
carre_valide([Carre|Q],Compteur) :- Compteur<4,ligne_valide(Carre),C is Compteur+1,carre_valide(Q,C).

carre_valide(Carre) :- aplatir_liste(Carre, Carref), ligne_valide(Carref).
% Aplatir une liste
aplatir_liste([], []).
aplatir_liste([T|Q], Res) :- aplatir_liste(T, TF), aplatir_liste(Q,QF), concat(TF, QF, Res), !.
aplatir_liste(T, [T]).

% Tester si une grille de sudoku est valide
sudoku_valide(Grille) :- grille_valide(Grille,1).

grille_valide(_,10) :- !.
grille_valide(Grille,Compteur) :- Compteur<10,retourner_ligne(Grille,Compteur,Ligne),ligne_valide(Ligne),
                                    retourner_colonne(Grille,Compteur,Colonne),colonne_valide(Colonne),
                                    retourner_carre(Grille,Compteur,Carre),carre_valide(Carre,1),C is Compteur+1,grille_valide(Grille,C).


% Vérifier que la grille est valide en considerant qu'une seule case a été modifiée (La case [Lig,Col]), permet de ne pas revérifier toute la grille à chaque mouvement
%ajout_valide(Grille, [Col, Lig, _]) :- retourner_ligne(Grille,Lig,Ligne), ligne_valide(Ligne),retourner_colonne(Grille,Col,Colonne),colonne_valide(Colonne), Idcar is 3*((Lig-1)//3)+((Col-1)//3)+1,retourner_carre(Grille,Idcar,Carre),carre_valide(Carre), !.

ajout_valide(Grille, [IdCol, IdLig, _]) :- retourner_ligne(Grille,IdLig,Ligne), ligne_valide(Ligne),retourner_colonne(Grille,IdCol,Colonne),colonne_valide(Colonne), IdCar is 3*((IdLig-1)//3)+((IdCol-1)//3)+1,retourner_carre(Grille,IdCar,Carre),carre_valide(Carre,1).
ajout_valide(_, [_, _, _]) :- write('/!\ Ajout incorrect'),nl,nl,fail.

% Vérifier que la coordonnée d'une case est valide
coordonnee_valide(X) :- X>0, X=<9, chiffre(X), !.
coordonnee_valide(_) :- nl, write('/!\ Coordonnee incorrecte'),nl,nl, fail.

% Tester si une grille de sudoku est pleine
grille_pleine([]).
grille_pleine([Liste|Reste]) :-­ \+ element_present(0,Liste), grille_pleine(Reste).



%===================================================%
%======== MODIFICATION D'UNE CASE DU SUDOKU ========%
%===================================================%

% Remplacer une valeur d'une liste par une autre
remplacer([_|Q], 1, Valeur, [Valeur|Q]) :- !.
remplacer([T|Q], Id, Valeur, [T|Res]) :- Id2 is Id-1, remplacer(Q, Id2, Valeur, Res).

% Remplacer une valeur d'une grille par une autre
remplacer_valeur(Grille, IdCol, IdLig, Valeur, Res) :- element_i(Grille, IdLig, Lig), remplacer(Lig, IdCol, Valeur, NewLig), remplacer(Grille, IdLig, NewLig, Res).

% Suppression d'une case
supprimer_valeur(Grille,IdColonne,IdLigne,G) :- remplacer_valeur(Grille,IdColonne,IdLigne,0,G).

% Vérifier que la case est vide
case_vide(Grille,I,J) :- \+case_pleine(Grille,I,J),!.
case_vide(_,_,_) :- write('/!\ Cette case est pleine'),nl,fail.

% Vérifier que la case est pleine
case_pleine(Grille,I,J) :- retourner_ligne(Grille,I,Ligne),element_i(Ligne,J,Valeur),Valeur =\= 0,!.
case_pleine(_,_,_) :- write('/!\ Cette case est vide'),nl,fail.



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
%============= RESOLUTION AUTOMATIQUE =============%
%==================================================%
%%%%%%%%% TENTATIVE %%%%%%%%%%%

% Tester si une case peut prendre une valeur
valeur_possible(Grille,I,J,Valeur) :- retourner_ligne(Grille,I,Ligne),\+ element_present(Ligne,Valeur),
									retourner_colonne(Grille,J,Colonne),\+ element_present(Colonne,Valeur), 
									IdCar is 3*((I-1)//3)+((J-1)//3)+1, retourner_carre(Grille,IdCar,Carre),aplatir_liste(Carre,Carre_plat),\+ element_present(Carre_plat,Valeur).

% Lister les valeurs possibles pour une case
valeurs_possibles(_,_,_,[],0) :- !.
valeurs_possibles(Grille,I,J,[Valeur|Reste],Valeur) :- valeur_possible(Grille,I,J,Valeur), !, V is Valeur-1, valeurs_possibles(Grille,I,J,Solution,V).
valeurs_possibles(Grille,I,J,Solution,Valeur) :- V is Valeur­-1, valeurs_possibles(Grille,I,J,Solution,V).

% Lister les valeurs possibles pour toutes les cases
toutes_valeurs(Grille,Solution) :- toutes_valeurs(Grille,Solution,1,1).
toutes_valeurs(Grille,[Solution],9,9) :- ­!,valeurs_possibles(Grille,9,9,Solution,9).
toutes_valeurs(Grille,[Solution],IdLigne,9) :- ­!,valeurs_possibles(Grille,IdLigne,9,Solution,9), I is IdLigne+1, toutes_valeurs(Grille,Solution,I,1).
toutes_valeurs(Grille,[Resultat|Solution],IdLigne,IdColonne) :- valeurs_possibles(Grille,IdLigne,IdColonne,Solution,9), I is IdColonne+1, toutes_valeurs(Grille,Solution,IdLigne,I).

% Appliquer les solutions pour les cases où il n'y a qu'une seule valeur possible
valeur_unique([[]],[],[[]]) :- !.
valeur_unique([[]|Reste1],Reste,[[]|Reste2]) :- !,valeur_unique(Reste1,Reste,Reste2).
valeur_unique([[0|Queue1]|Reste1],[[X]|Reste],[[X|Queue2]|Reste2]) :- !,valeur_unique([Queue1|Reste1],Reste,[Queue2|Reste2]).
valeur_unique([[Tete|Queue1]|Reste1],[_|Reste],[[Tete|Queue2]|Reste2]) :- !,valeur_unique([Queue1|Reste1],Reste,[Queue2|Reste2]).

% Tester si une valeur est unique dans une liste, et renvoyer sa position
unique(X,[X|T],1) :- !, \+ element_present(X,T).
unique(X,[_|T],Indice) :- I is Indice+1,unique(X,T,I).

%%%%% Résolution automatique
solve(Grille) :- sudoku_valide(Grille),afficher_grille(Grille),nl,toutes_valeurs(Grille,Solution), solve(Grille,Solution).
% Condition d'arrêt
solve(Grille,Solution) :- \+ element_present(Solution,[_|_]), grille_pleine(Grille), sudoku_valide(Grille), !, nl, afficher_grille(Grille).
% Application des solutions uniques pour chaque case où il n'y a qu'une seule valeur possible
solve(Grille,Solution) :- element_present(Solution,[_]), !, valeur_unique(Grille,Solution,Sudoku), sudoku_valide(Sudoku), toutes_valeurs(Sudoku,Resultat), solve(Sudoku,Resultat).
% Application unique
%solve(Grille,Solution) :- unique(



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
		write('2. Entrer un sudoku manuellement'),nl,
		write('3. Quitter'),nl,nl,
			read(Choix), nl,
			cas(Choix),
			Choix = 3, nl.

cas(1) :- write(' %===== RESOLUTION =====%'),nl,
			sudoku(4,Grille),
			asserta(grille(Grille)),
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
	grille(Grille), 
	afficher_grille(Grille), nl,
	write('1. Entrer un chiffre'), nl,
	write('2. Supprimer un chiffre'), nl,
	write('3. Resoudre automatiquement'), nl,
	write('4. Quitter'), nl, nl,
	read(Choix), nl,
	cas_resolution(Choix,Grille),
	grille(Grille_modifiee),
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
	remplacer_valeur(Grille,IdColonne,IdLigne,Valeur,G),
	sudoku_valide(G),
	retract(grille(Grille)),
	asserta(grille(G)),
	write('Valeur ajoutee'),nl,nl,!.

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
	retract(grille(Grille)),
	asserta(grille(G)),
	write('Valeur supprimee'),nl,nl,!.

cas_resolution(2,_) :- !.

% Résolution automatique
cas_resolution(3,Grille) :- write('Tentative infructueuse de resoudre automatiquement un sudoku'),!.

cas_resolution(2,_) :- !.

% Quitter
cas_resolution(4,_) :- !.

cas_resolution(_,_) :- nl, write('/!\ Entrez 1,2,3 ou 4'), nl, !, fail.


%---------------------------------------------------%
%------------------- PROPOSITION -------------------%
%---------------------------------------------------%

% Menu proposition
entrer_sudoku :- nl,write('  %=== ENTREZ LE SUDOKU ===%'),nl,nl,
	proposition_sudoku(Grille),
	afficher_grille(Grille), nl,
	write('1. Entrer un chiffre'), nl,
	write('2. Supprimer un chiffre'), nl,
	write('3. Enregistrer le sudoku'), nl,
	write('4. Quitter'), nl, nl,
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
	remplacer_valeur(Grille,IdColonne,IdLigne,Valeur,G),
	sudoku_valide(G),
	retract(proposition_sudoku(Grille)),
	asserta(proposition_sudoku(G)),
	write('Valeur ajoutee'),nl,nl,!.

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

% Valider un sudoku
cas_proposition(3,Grille) :- write('Sudoku valide'),nl,nl, afficher_grille(Grille),nl,nl.

cas_proposition(3,_) :- !.

% Quitter
cas_proposition(4,_) :- !.

cas_proposition(_,_) :- nl, write('/!\ Entrez 1,2,3 ou 4'), nl, !, fail.
