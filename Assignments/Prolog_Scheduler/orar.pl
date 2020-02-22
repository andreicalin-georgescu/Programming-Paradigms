
:-ensure_loaded('probleme.pl').
:-ensure_loaded('testing.pl').


% predicat pentru accesarea listei de staff din problema
get_staff(Context, Staff) :-
	member(Staff_element, Context),
	Staff_element = staff(Staff).

% predicat pentru accesarea listei de zile disponibile din problema
get_days(Context, Days) :-
	member(Days_element, Context),
	Days_element = days(Days).

% predicat pentru accesarea listei de intervale disponibile
get_times(Context, Times) :-
	member(Times_element, Context),
	Times_element = times(Times).

% predicat pentru accesarea listei de sali disponibile
get_rooms(Context, Rooms) :-
	member(Rooms_element, Context),
	Rooms_element = rooms(Rooms).

% predicat pentru accesarea grupelor 
get_groups(Context, Groups) :-
	member(Groups_element, Context),
	Groups_element = groups(Groups).

% predicat pentru accesarea activitatilor 
get_activities(Context, Activities) :-
	member(Activities_element, Context),
	Activities_element = activities(Activities).

% predicat pentru accesarea constrangerilor
get_constraints(Context, Constraints) :-
	member(Constraints_element, Context),
	Constraints_element = constraints(Constraints).

% schedule(+Context, -Sol)
% pentru contextul descris, întoarce o soluție care respectă
% constrângerile fizice și de curiculă.
schedule(Context, (Context, [])) :- (get_staff(Context, Staff_element), length(Staff_element, 0));
                                    (get_groups(Context, Groups_element), length(Groups_element, 0));
                                    (get_rooms(Context, Rooms_element), length(Rooms_element, 0));
                                    (get_times(Context, [(_, Dimensiune) | _]), get_activities(Context, [(_, Times) | _]), Dimensiune < Times).

% cost(+Sol, -Cost)
% pentru soluția dată, întoarce costul implicat de constrângerile de
% preferință care au fost încălcate.
cost(_, _) :- fail.

% schedule_best(+Context, -Sol, -Cost)
% pentru contextul descris, întoarce soluția validă cu cel mai bun (cel
% mai mic) cost (sau una dintre ele, dacă există mai multe cu același
% cost)
schedule_best(_, _, _) :- fail.













