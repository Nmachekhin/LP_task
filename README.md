Задача Вовк, Коза, Капуста:

Реалізація на prolog:

transfer(l, r), transfer(r, l) - предикати зміни берега

move([Man, Wolf, Goat, Cabbage], action, [Man, Wolf, Goat, Cabbage]) - предикат переміщення об'єктів на інший берег, дія вказує на тип переміщення

safeState([Man, Wolf, Goat, Cabbage]) - предикат перевірки стану на несуперечливість умові задачі

solution(State, PrevStates, Moves) - рекурсивний предикат пошуку розв'язку задачі за допомогою пошуку в глибину. Зберігає попередні стани для запобіганню зациклення.

allSolutions(InitialState, Solutions) - предикат пошуку усіх можливих розв'язків задачі.






Задача 3 сімейні пари:

initial_state(state) - предикат що вказує на те, чи є стан початковим для задачі.

goal_state(state) - предикат перевірки на те що поточний стан є кінцевим

safe_bank(H, W) - перевіряє стан одного берега на несуперечливість умові задачі.

move(InitialState, NextState) - предикат переміщення об'єктів на інший берег, дозволяє лиш ті переміщення, що не суперечать умові про знаходження людей в човні, конкретизується для двох положень човна.

select_passengers(H, W, Passangers) - предикат що дозволяє отримати всі можливі валідні комбінації людей в човні, впорядковані і не суперечливі до умови. Пасажири вибираються зі вказанням статі.

update_banks(Passengers, H1, W1, H2, W2, H1New, W1New, H2New, W2New) - предикат що дозволяє здійснити переміщення вказаних пасажирів з одного берега на інший.

remove_people(Passangers, H, W, HNew, WNew) - предикат що видалє вказаних людей з вказаного берега.

add_people(Passangers, H, W, HNew, WNew) - предикат що додає людей на вказаний берег.

bfs(Paths, Visited, FinalPath) - рекурсивний предикат пошуку в ширину, приймає чергу згенерованих маршрутів, усі вже відвідані стани (для запобіганюю зациклення) та маршрут який є розв'язком задачі.

solve(Path) - предикат що викликає пошук ровз'язку з початкового стану та розвертає список з отриманим шляхом для зручнішого представлення.

Реалізація мовою haskell майже повністю збігається з реалізацією мовою prolog.
