DECLARE SUB Set (kid%, kanti%, id$, anti$, b$)
DECLARE SUB info ()
DECLARE SUB Statistic (kto$, sx%, so%)
DECLARE SUB PoleIgr ()
DECLARE SUB GlavMenu ()
DECLARE SUB AnalisMas (px%, py%, os$())
DIM p(1 TO 8, 1 TO 8) AS STRING * 1, os(1 TO 8, 1 TO 8) AS STRING * 1

DATA a,k,n,t,t,x,1,2                 'инициализация os$()
DATA b,k,n,u,u,h,2,1
DATA c,b,o,p,h,y,y,6
DATA c,e,p,o,j,v,v,6
DATA e,l,l,d,q,j,3,4
DATA f,m,m,d,q,z,4,3
DATA g,f,r,s,z,w,5,g
DATA i,a,s,r,x,w,5,i

FOR i% = 1 TO 8
    FOR j% = 1 TO 8
    READ os$(i%, j%)
            'PRINT os$(i%, j%); " ";  'отладка
    NEXT j%
            'PRINT                    'отладка
NEXT i%

DO                          'цикл партий (бесконечный)
    'malx% = 0: malo% = 0
lab:    LOCATE 1, 1, 0       'невидимый курсор
        GlavMenu                'пп главного меню,выхода и информации
                            'реинициализация p$()
    FOR i% = 1 TO 8
        FOR j% = 1 TO 8
        p$(i%, j%) = " "
        NEXT j%
    NEXT i%
   
    RANDOMIZE TIMER         'иниц-я базы случ чисел
    sl% = RND * 10          'генер случ числа-сколько раз преобраз-ть os$()
            PRINT sl%
    FOR m% = 1 TO sl%       'цикл преобразований os$()
        IF (m% MOD 2 = 0) THEN       'диагональная ось
            FOR j% = 2 TO 8          'счётчик столбцов
                FOR i% = 1 TO 8      'счётчик строк
                    IF i% < j% THEN
                    SWAP os$(i%, j%), os$(j%, i%)
                    ELSE
                    EXIT FOR
                    END IF
                NEXT i%
            NEXT j%
        ELSE                        'горизонтальная ось
            FOR j% = 1 TO 8
                FOR i% = 1 TO 4
                SWAP os$(i%, j%), os$(9 - i%, j%)
                NEXT i%
            NEXT j%
        END IF
    NEXT m%
    PoleIgr                 'пп отрисовки игрового поля
    kto$ = ""               'пп вывода счёта,сообщения о выйгрыше и т.п.
    Statistic kto$, sx%, so%     ', (sx% * 100 / sx% + so%), (so% * 100 / sx% + so%)

    'ЦИКЛ ИГРОКА**********************************************************
    xo% = 3: yo% = 16       'координаты левого верхнего угла
    px% = 1: py% = 1        'начальные координаты(<>0)
    zn$ = "X"               'первым ходит пользователь крестиком
   
    FOR xod% = 1 TO 64      'тот самый цикл(универсальный для обоих)
    '---------------------------------------------------------
           'сканирование и определение
           ' максимальной альтернативы
    MaxAl% = -1
    id$ = "X": anti$ = "O"      'сначала для "X"
FOR r% = 1 TO 2                 'r% управляет,какие id$ и anti$
FOR k% = 1 TO 4                 'k% управляет,какие альтернативы сканировать
    FOR i% = 1 TO 8             'i%-переходит к следующей альтернативе
    kid% = 0: kanti% = 0        'обнуление перед каждой альтернативой
        FOR j% = 1 TO 8         'j%-пробегает по значкам альтернативы
        SELECT CASE k%
        CASE 1                  'сканирование по строкам
            Set kid%, kanti%, id$, anti$, p$(i%, j%)
        CASE 2                  'по столбцам
            Set kid%, kanti%, id$, anti$, p$(j%, i%)
        CASE 3                  'по главной диагонали (8 раз,знаю)
            Set kid%, kanti%, id$, anti$, p$(j%, j%)
        CASE 4                  'по другой диагонали(-//-)
            Set kid%, kanti%, id$, anti$, p$(9 - j%, j%)
        END SELECT
        NEXT j%
  
    IF kanti% <> 0 THEN         'если кол-во значков противника <>0,
        TekAl% = -1             ' значит эта альт-ва отпадает(=-1)
    ELSE
        TekAl% = kid%           'пустая строка-8 пробелов,не пустая- кол-во
    END IF                          'своих значков
  
    IF TekAl% > MaxAl% THEN MaxAl% = TekAl%       'запоминание максималной
                                                    'альтернативы
    NEXT i%

NEXT k%
    SELECT CASE r%
    CASE 1
        malx% = MaxAl%
    CASE 2
        malo% = MaxAl%
    END SELECT
    id$ = "O": anti$ = "X"          'второй проход r%-для "O"
NEXT r%

'LOCATE 1, 1: PRINT "malx%="; malx%, "malo%="; malo%: SLEEP
'-----------------------------------------------------------------------
        'проверка на окончание игры
  IF malx% = 8 THEN kto$ = " ПОБЕДА X ! ": EXIT FOR          'чистая победа Х
  IF malo% = 8 THEN kto$ = " ПОБЕДА O ! ": EXIT FOR          'чистая победа О
  IF malx% = -1 THEN kto$ = " МАТ Х-ку ! ": EXIT FOR  'это мат Х-ку
  IF malo% = -1 THEN kto$ = " МАТ О-ку ! ": EXIT FOR  'это мат О-ку
  IF malx% = -1 AND malo% = -1 THEN kto$ = "   НИЧЬЯ .  ": EXIT FOR 'ничья
       
        COLOR 15, 2             'цвет сообщения "ВАШ ХОД"
        'точка ветвления
        SELECT CASE zn$
        '------------------------------------------------------------------
        CASE "X"            'ход пользователя
        LOCATE 13, 4: PRINT " ВАШ ХОД "
     DO
     kl$ = ""                       'цикл перемещения курсора
        DO WHILE kl$ <> CHR$(13)
            kl$ = INKEY$
        LOCATE xo% + px% * 2, yo% + py% * 2, 1, 0, 31
        rig$ = RIGHT$(kl$, 1)
            IF kl$ = CHR$(27) THEN GOTO lab
            IF rig$ = "H" AND px% <> 1 THEN px% = px% - 1
            IF rig$ = "P" AND px% <> 8 THEN px% = px% + 1
            IF rig$ = "K" AND py% <> 1 THEN py% = py% - 1
            IF rig$ = "M" AND py% <> 8 THEN py% = py% + 1
        LOOP
            'возвращаются координаты в матрице
     LOOP UNTIL p$(px%, py%) = " "
         ppx% = px%: ppy% = py%
         '-------------------------------------------------------------------
        CASE "O"       'ход компьютера
           
            LOCATE 13, 5: PRINT "МОЙ ХОД"
    
            FOR i% = 1 TO 8
                FOR j% = 1 TO 8
                IF p$(i%, j%) = " " THEN        'свободно ли место
                    SELECT CASE os$(i%, j%)
                    CASE os$(ppx%, ppy%)          'совпадает ли идент-р ячейки
                            px% = i%
                            py% = j%
                            GOTO lab2
                    END SELECT
                END IF
                NEXT j%
            NEXT i%
            '---------------------------------------------------------------
        END SELECT
       
        'печать X или O
lab2:
        LOCATE xo% + 2 * px%, yo% + 2 * py%
        COLOR 15, 1             'цвет значков на поле
        PRINT zn$
       
        'занесение хода в p$()
        p$(px%, py%) = zn$
       
        'переопределение zn$,а значит и того, кто ходит
        SELECT CASE zn$
        CASE "X"
            zn$ = "O"
        CASE "O"
            zn$ = "X"
        END SELECT
    NEXT xod%
    '*********************************************************************
   
    SELECT CASE kto$         'подсчёт счёта
    CASE " ПОБЕДА X ! "
        sx% = sx% + 1
    CASE " МАТ О-ку ! "
        sx% = sx% + 1
    CASE " ПОБЕДА O ! "
        so% = so% + 1
    CASE " МАТ Х-ку ! "
        so% = so% + 1
    END SELECT
    Statistic kto$, sx%, so%
    LOCATE 1, 1, 0       'невидимый курсор
    SLEEP
LOOP

SUB GlavMenu
'подпрограмма главного меню для игры "ШАРИКИ-РОЛИКИ"

DIM m(1 TO 3) AS STRING * 10
m$(1) = "ИГРА      "
m$(2) = "ИНФОРМАЦИЯ"
m$(3) = "ВЫХОД     "
    WIDTH 40, 25
    s% = 1: stz% = 2
nac:
    COLOR 1, 0                             'печать фона
    FOR i% = 1 TO 1000
    PRINT CHR$(206);    '249 ,176,196
    NEXT i%
 LOCATE 23, 3: PRINT CHR$(176); CHR$(177); STRING$(7, 178); CHR$(177); CHR$(176)
 'лейбл
  COLOR 3, 1
  LOCATE 23, 6: PRINT " VJV "
  
    LOCATE 1, 1

     COLOR 10, 2                             'заголовок
      PRINT STRING$(40, 176)
      PRINT STRING$(4, 176); STRING$(32, 177); STRING$(4, 176);
      FOR i% = 1 TO 3
      PRINT STRING$(4, 176); STRING$(4, 177); STRING$(24, 178); STRING$(4, 177); STRING$(4, 176)
      NEXT i%
      PRINT STRING$(4, 176); STRING$(32, 177); STRING$(4, 176);
      PRINT STRING$(40, 176)
      LOCATE 4, 11: COLOR 14, 1
      PRINT " ШАРИКИ-РОЛИКИ  v.1 "
                          
                            'печать главного меню
COLOR 10, 2                     'печать рамки
    LOCATE 9, 6: PRINT STRING$(30, 176)
    LOCATE 10, 6: PRINT CHR$(176); STRING$(28, 177); CHR$(176)
    FOR i% = 11 TO 19
     LOCATE i%, 6
     PRINT CHR$(176); CHR$(177); STRING$(26, 178); CHR$(177); CHR$(176)
    NEXT i%
    LOCATE 20, 6: PRINT CHR$(176); STRING$(28, 177); CHR$(176)
    LOCATE 21, 6: PRINT STRING$(30, 176)
 
COLOR 15, 1                          'печать пунктов меню
  FOR i% = 1 TO 3
  LOCATE 11 + i% * 2, 13      '14
  PRINT "   "; m$(i%); "   "
  NEXT i%

     kl$ = ""                       'цикл меню
     DO WHILE kl$ <> CHR$(13)
     COLOR 30, 1
     LOCATE 11 + s% * 2, 13
     PRINT CHR$(17); CHR$(4); CHR$(16)
     LOCATE 11 + stz% * 2, 13
     PRINT "   "
        DO                              'цикл ожидания
        kl$ = INKEY$
        LOOP WHILE kl$ = ""
        IF RIGHT$(kl$, 1) = "H" AND s% <> 1 THEN stz% = s%: s% = s% - 1: SOUND 40, 1
        IF RIGHT$(kl$, 1) = "P" AND s% <> 3 THEN stz% = s%: s% = s% + 1: SOUND 40, 1
    LOOP
     ON s% GOTO L1, L2, L3     '"диспетчер" гл.меню

L1:
    SOUND 80, 2
    EXIT SUB
L2:
    SOUND 50, 2
    info
    GOTO nac
L3:
SOUND 100, 1
SOUND 0, 1
SOUND 100, 1
SOUND 0, 1
SOUND 60, 1
SOUND 0, 1
 COLOR 7, 0
 END

END SUB

SUB info
DIM s(16) AS STRING * 30
     s$(0) = CHR$(201) + STRING$(28, 205) + CHR$(187)
     s$(1) = CHR$(186) + "    ИГРА 'ШАРИКИ-РОЛИКИ'    " + CHR$(186)
     s$(2) = CHR$(186) + "  Правила этой игры ничем не" + CHR$(186)
     s$(3) = CHR$(186) + "отличаются от правил обычных" + CHR$(186)
     s$(4) = CHR$(186) + "КРЕСТИКОВ-НОЛИКОВ. Вся  осо-" + CHR$(186)
     s$(5) = CHR$(186) + "бенность предлогаемой игры в" + CHR$(186)
     s$(6) = CHR$(186) + "том , что всё происходит на " + CHR$(186)
     s$(7) = CHR$(186) + "поле 8 x 8 клеточек. Для по-" + CHR$(186)
     s$(8) = CHR$(186) + "беды требуется установить в " + CHR$(186)
     s$(9) = CHR$(186) + "один ряд 8 ноликов или крес-" + CHR$(186)
    s$(10) = CHR$(186) + "тиков на вертикали, горизон-" + CHR$(186)
    s$(11) = CHR$(186) + "тали или диагонали.         " + CHR$(186)
    s$(12) = CHR$(186) + "                            " + CHR$(186)
    s$(13) = CHR$(186) + "elenexus@mail.ru            " + CHR$(186)
    s$(14) = CHR$(186) + "(C)CorvusLab. v.1 07.VI.2000" + CHR$(186)
    s$(15) = CHR$(200) + STRING$(28, 205) + CHR$(188)

    COLOR 1, 0                             'печать фона
    FOR i% = 1 TO 1000
    PRINT CHR$(206);    '249 ,176,196
    NEXT i%
    
COLOR 10, 2                                 'печать текста
    LOCATE 2, 1: PRINT STRING$(40, 176)
    PRINT STRING$(40, 177)
    PRINT STRING$(40, 178)
    PRINT STRING$(40, 178)
    FOR i% = 5 TO 20
    COLOR 15, 2: LOCATE i%, 6
    PRINT s$(i% - 5)
    COLOR 10, 2
    PRINT STRING$(40, 178)
    PRINT STRING$(40, 177)
    PRINT STRING$(40, 176)
    SOUND 40, 1
    NEXT i%

SLEEP
END SUB

SUB PoleIgr
COLOR 1, 0                             'печать фона
    FOR i% = 1 TO 1000
    PRINT CHR$(206);    '249 ,176,196
    NEXT i%
 'лейбл
  LOCATE 23, 3: PRINT CHR$(176); CHR$(177); STRING$(7, 178); CHR$(177); CHR$(176)
  COLOR 3, 1
  LOCATE 23, 6: PRINT " VJV "

    'печать рамки игрового поля
COLOR 10, 2
LOCATE 2, 15: PRINT STRING$(21, 176)
LOCATE 3, 15: PRINT CHR$(176); STRING$(19, 178); CHR$(176)
FOR i% = 4 TO 20
    LOCATE i%, 15
    PRINT CHR$(176); STRING$(19, 178); CHR$(176)
NEXT i%
LOCATE 21, 15: PRINT CHR$(176); STRING$(19, 178); CHR$(176)
LOCATE 22, 15: PRINT STRING$(21, 176)
  
 'окошки для статистики

LOCATE 3, 3: PRINT STRING$(11, 176)
FOR i% = 4 TO 8
    LOCATE i%, 3
    PRINT CHR$(176); STRING$(9, 178); CHR$(176)
NEXT i%
LOCATE 9, 3: PRINT STRING$(11, 176)

LOCATE 11, 3: PRINT STRING$(11, 176)
FOR i% = 12 TO 14
    LOCATE i%, 3
    PRINT CHR$(176); STRING$(9, 178); CHR$(176)
NEXT i%
LOCATE 15, 3: PRINT STRING$(11, 176)

COLOR 14, 2
    LOCATE 5, 5: PRINT " X:    "
    LOCATE 7, 5: PRINT " O:    "
               ' LOCATE 15, 3: PRINT " НИКТО НЕ "    'отладка
               ' LOCATE 17, 4: PRINT " ВЫЙГРАЛ "     'отладка
     'печать клеточек
 COLOR 11, 1
 LOCATE 4, 17     'верхняя строка
    PRINT CHR$(201);
        FOR i% = 1 TO 8
            PRINT CHR$(205); CHR$(209);
        NEXT i%
    LOCATE 4, 33: PRINT CHR$(187)

 FOR i% = 5 TO 19           'строки средние
    LOCATE i%, 17
    PRINT CHR$(186);        'строка с местами для крестиков/ноликов
        FOR j% = 1 TO 8
            PRINT CHR$(32); CHR$(179);
        NEXT j%
    LOCATE i%, 33: PRINT CHR$(186)
    i% = i% + 1
    LOCATE i%, 17           'строка разделительная
    PRINT CHR$(199);
        FOR j% = 1 TO 8
            PRINT CHR$(196); CHR$(197);
        NEXT j%
    LOCATE i%, 33: PRINT CHR$(182)
 NEXT i%

 LOCATE 20, 17     'нижняя строка
    PRINT CHR$(200);
        FOR i% = 1 TO 8
            PRINT CHR$(205); CHR$(207);
        NEXT i%
    LOCATE 20, 33: PRINT CHR$(188)

    'печать подсказок о клавишах
COLOR 9, 1
LOCATE 19, 3: PRINT "     Enter "
LOCATE 20, 3: PRINT "           "
LOCATE 21, 3: PRINT "     Esc   "
DEF SEG = &HB800
POKE 1448, 24
POKE 1526, 27
POKE 1528, 254
POKE 1530, 26
POKE 1608, 25

END SUB

SUB Set (kid%, kanti%, id$, anti$, b$)
SELECT CASE b$
CASE id$
    kid% = kid% + 1
CASE anti$
    kanti% = kanti% + 1
END SELECT
END SUB

SUB Statistic (k$, sx%, so%)
COLOR 15, 2                 'отражение счёта
LOCATE 5, 8: PRINT sx%
LOCATE 7, 8: PRINT so%

IF k$ <> "" THEN            'сообщение о выйгрыше
SOUND 150, 2
COLOR 4, 2
LOCATE 10, 2: PRINT STRING$(16, 219)
FOR i% = 11 TO 13
    LOCATE i%, 2: COLOR 4, 2
    PRINT CHR$(219); STRING$(14, 32); CHR$(219)
    LOCATE i%, 3: COLOR 0, 0: PRINT STRING$(14, 32)
NEXT i%
COLOR 4, 2
LOCATE 14, 2: PRINT STRING$(16, 219)
COLOR 15, 0
LOCATE 12, 4: PRINT k$; CHR$(16)
END IF

END SUB

