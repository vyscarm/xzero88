DECLARE SUB GOs (os$())
DECLARE SUB DiOs (os$())
DECLARE SUB Set (kid%, kanti%, id$, anti$, b$)
DECLARE FUNCTION MaxAl% (p$(), id$, anti$)
DECLARE SUB info ()
DECLARE SUB Statistic (kto$, sx%, so%)
DECLARE SUB PoleIgr ()
DECLARE SUB GlavMenu ()
DECLARE SUB VvodPolz (px%, py%, p$())
DECLARE SUB AnalisMas (px%, py%, os$(), p$())
DIM p$(1 TO 8, 1 TO 8)' AS STRING * 1,
DIM os$(1 TO 8, 1 TO 8)' AS STRING * 1

DATA a,k,n,t,t,x,1,2                 '���樠������ os$()
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
    NEXT j%
NEXT i%

DO                          '横� ���⨩ (��᪮����)
        GlavMenu                '�� �������� ����,��室� � ���ଠ樨
              '२��樠������ p$()
    FOR i% = 1 TO 8
        FOR j% = 1 TO 8
        p$(i%, j%) = " "
        NEXT j%
    NEXT i%

    RANDOMIZE TIMER         '����-� ���� ��� �ᥫ
    sl% = RND * 10          '����� ��� �᫠-᪮�쪮 ࠧ �८�ࠧ-�� os$()
    FOR m% = 1 TO sl%       '横� �८�ࠧ������ os$()
        IF (m% MOD 2 = 0) THEN       '��������쭠� ���
         DiOs os$()
        ELSE                        '��ਧ��⠫쭠� ���
         GOs os$()
        END IF
    NEXT m%
    PoleIgr                 '�� ���ᮢ�� ��஢��� ����
    kto$ = ""                  '�� �뢮�� ����,ᮮ�饭�� � �멣��� � �.�.
    Statistic kto$, sx%, so%

    '���� ������**********************************************************
    px% = 1: py% = 1        '��砫�� ���न����(<>0)
    zn$ = "X"               '���� 室�� ���짮��⥫� ���⨪��
  
    FOR i% = 1 TO 64        '�� ᠬ� 横�(㭨���ᠫ�� ��� �����)
        malx% = MaxAl%(p$(), "X", "O")    '᪠��஢���� � ��।������
        malo% = MaxAl%(p$(), "O", "X")    ' ���ᨬ��쭮� ����ୠ⨢�
       
        '�஢�ઠ �� ����砭�� ����
  IF malx% = 8 THEN kto$ = " ������ X ! ": EXIT FOR          '���� ������ �
  IF malo% = 8 THEN kto$ = " ������ O ! ": EXIT FOR          '���� ������ �
  IF malx% = -1 AND malo% = -1 THEN kto$ = "   ����� .  ": EXIT FOR '�����
  IF malx% = -1 THEN kto$ = " ��� �-�� ! ": EXIT FOR  '�� ��� �-��
  IF malo% = -1 THEN kto$ = " ��� �-�� ! ": EXIT FOR  '�� ��� �-��
        COLOR 15, 2         '梥� ᮮ�饭�� "��� ���"
        '�窠 ��⢫����
        SELECT CASE zn$
        CASE "X"
            VvodPolz px%, py%, p$()
        CASE "O"
            AnalisMas px%, py%, os$(), p$()
        END SELECT
        '����� X ��� O
        LOCATE 3 + 2 * px%, 16 + 2 * py%
        COLOR 15, 1             '梥� ���窮� �� ����
        PRINT zn$
        '����ᥭ�� 室� � p$()
        p$(px%, py%) = zn$
        '��८�।������ zn$,� ����� � ⮣�, �� 室��
        SELECT CASE zn$
        CASE "X"
            zn$ = "O"
        CASE "O"
            zn$ = "X"
        END SELECT
    NEXT i%
    '*********************************************************************
   
    SELECT CASE kto$         '������� ����
    CASE " ������ X ! "
        sx% = sx% + 1
    CASE " ��� �-�� ! "
        sx% = sx% + 1
    CASE " ������ O ! "
        so% = so% + 1
    CASE " ��� �-�� ! "
        so% = so% + 1
    END SELECT
    Statistic kto$, sx%, so%
    SLEEP

LOOP

SUB AnalisMas (px%, py%, os$(), p$())
            LOCATE 13, 5: PRINT "��� ���"
   
            FOR i% = 1 TO 8
                FOR j% = 1 TO 8
                IF p$(i%, j%) = " " THEN        '᢮����� �� ����
                    SELECT CASE os$(i%, j%)
                    CASE os$(px%, py%)          'ᮢ������ �� �����-� �祩��
                            px% = i%
                            py% = j%
                            EXIT SUB
                    END SELECT
                END IF
                NEXT j%
            NEXT i%
END SUB

SUB DiOs (os$())
    FOR j% = 2 TO 8          '����稪 �⮫�殢
         FOR i% = 1 TO 8      '����稪 ��ப
             IF i% < j% THEN
             SWAP os$(i%, j%), os$(j%, i%)
             ELSE
             EXIT FOR
             END IF
          NEXT i%
     NEXT j%
END SUB

SUB GlavMenu
'����ணࠬ�� �������� ���� ��� ���� "������-������"

DIM m(1 TO 3) AS STRING * 10
m$(1) = "����      "
m$(2) = "����������"
m$(3) = "�����     "
    WIDTH 40, 25
    s% = 1: stz% = 2
nac:
    COLOR 1, 0                             '����� 䮭�
    FOR i% = 1 TO 1000
    PRINT CHR$(206);    '249 ,176,196
    NEXT i%
 LOCATE 23, 29: PRINT CHR$(176); CHR$(177); STRING$(7, 178); CHR$(177); CHR$(176)
 '�����
  COLOR 3, 1
  LOCATE 23, 32: PRINT " VJV "
  
    LOCATE 1, 1

     COLOR 10, 2                             '���������
      PRINT STRING$(40, 176)
      PRINT STRING$(4, 176); STRING$(32, 177); STRING$(4, 176);
      FOR i% = 1 TO 3
      PRINT STRING$(4, 176); STRING$(4, 177); STRING$(24, 178); STRING$(4, 177); STRING$(4, 176)
      NEXT i%
      PRINT STRING$(4, 176); STRING$(32, 177); STRING$(4, 176);
      PRINT STRING$(40, 176)
      LOCATE 4, 11: COLOR 14, 1
      PRINT " ������-������  v.1 "
                          
                            '����� �������� ����
COLOR 10, 2                     '����� ࠬ��
    LOCATE 9, 6: PRINT STRING$(30, 176)
    LOCATE 10, 6: PRINT CHR$(176); STRING$(28, 177); CHR$(176)
    FOR i% = 11 TO 19
     LOCATE i%, 6
     PRINT CHR$(176); CHR$(177); STRING$(26, 178); CHR$(177); CHR$(176)
    NEXT i%
    LOCATE 20, 6: PRINT CHR$(176); STRING$(28, 177); CHR$(176)
    LOCATE 21, 6: PRINT STRING$(30, 176)
 
COLOR 15, 1                          '����� �㭪⮢ ����
  FOR i% = 1 TO 3
  LOCATE 11 + i% * 2, 13      '14
  PRINT "   "; m$(i%); "   "
  NEXT i%

     kl$ = ""                       '横� ����
     DO WHILE kl$ <> CHR$(13)
     COLOR 30, 1
     LOCATE 11 + s% * 2, 13
     PRINT CHR$(17); CHR$(4); CHR$(16)
     LOCATE 11 + stz% * 2, 13
     PRINT "   "
        DO                              '横� ��������
        kl$ = INKEY$
        LOOP WHILE kl$ = ""
        IF RIGHT$(kl$, 1) = "H" AND s% <> 1 THEN stz% = s%: s% = s% - 1: SOUND 40, 1
        IF RIGHT$(kl$, 1) = "P" AND s% <> 3 THEN stz% = s%: s% = s% + 1: SOUND 40, 1
    LOOP
     ON s% GOTO L1, L2, L3     '"��ᯥ���" ��.����

L1:
    SOUND 80, 2
    EXIT SUB
L2:
    SOUND 50, 2
    info
    GOTO nac
L3:
COLOR 7, 0
END
END SUB

SUB GOs (os$())
   FOR j% = 1 TO 8
       FOR i% = 1 TO 4
       SWAP os$(i%, j%), os$(9 - i%, j%)
       NEXT i%
   NEXT j%
END SUB

SUB info
DIM s(16) AS STRING * 30
     s$(0) = "                              "
     s$(1) = "     ���� '������-������'     "
     s$(2) = "   �ࠢ��� �⮩ ���� ��祬 �� "
     s$(3) = " �⫨����� �� �ࠢ�� ������ "
     s$(4) = " ���������-�������. ���  ��- "
     s$(5) = " �������� �।�������� ���� � "
     s$(6) = " ⮬ , �� ��� �ந�室�� ��  "
     s$(7) = " ���� 8 x 8 ����祪. ��� ��- "
     s$(8) = " ���� �ॡ���� ��⠭����� �  "
     s$(9) = " ���� �� 8 ������� ��� ���- "
    s$(10) = " ⨪�� �� ���⨪���, ��ਧ��- "
    s$(11) = " ⠫� ��� ���������.          "
    s$(12) = "                              "
    s$(13) = " elenexus@mail.ru             "
    s$(14) = " (C)CorvusLab. v.1 07.VI.2000 "
    s$(15) = "                              "

    COLOR 1, 0                             '����� 䮭�
    FOR i% = 1 TO 1000
    PRINT CHR$(206);    '249 ,176,196
    NEXT i%
   
COLOR 10, 2                                 '����� ⥪��
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

FUNCTION MaxAl% (p$(), id$, anti$)
    MaxAl% = -1
    MAl% = -1
FOR k% = 1 TO 4                 'k% �ࠢ���,����� ����ୠ⨢� ᪠��஢���
    FOR i% = 1 TO 8             'i%-���室�� � ᫥���饩 ����ୠ⨢�
    kid% = 0: kanti% = 0        '���㫥��� ��। ������ ����ୠ⨢��
        FOR j% = 1 TO 8         'j%-�஡����� �� ���窠� ����ୠ⨢�
        SELECT CASE k%
        CASE 1                  '᪠��஢���� �� ��ப��
            Set kid%, kanti%, id$, anti$, p$(i%, j%)
        CASE 2                  '�� �⮫�栬
            Set kid%, kanti%, id$, anti$, p$(j%, i%)
        CASE 3                  '�� ������� ��������� (8 ࠧ,����)
            Set kid%, kanti%, id$, anti$, p$(j%, j%)
        CASE 4                  '�� ��㣮� ���������(-//-)
            Set kid%, kanti%, id$, anti$, p$(9 - j%, j%)
        END SELECT
        NEXT j%
 
    IF kanti% <> 0 THEN         '�᫨ ���-�� ���窮� ��⨢���� <>0,
        TekAl% = -1             ' ����� �� ����-�� �⯠����(=-1)
    ELSE
        TekAl% = kid%           '����� ��ப�-8 �஡����,�� �����- ���-��
    END IF                          '᢮�� ���窮�
    
    IF TekAl% > MAl% THEN MAl% = TekAl%       '����������� ���ᨬ�����
                                                    '����ୠ⨢�
    NEXT i%

NEXT k%
MaxAl% = MAl%
END FUNCTION

SUB PoleIgr
COLOR 1, 0                             '����� 䮭�
    FOR i% = 1 TO 1000
    PRINT CHR$(206);    '249 ,176,196
    NEXT i%
 '�����
  LOCATE 23, 3: PRINT CHR$(176); CHR$(177); STRING$(7, 178); CHR$(177); CHR$(176)
  COLOR 3, 1
  LOCATE 23, 6: PRINT " VJV "

    '����� ࠬ�� ��஢��� ����
COLOR 10, 2
LOCATE 2, 15: PRINT STRING$(21, 176)
LOCATE 3, 15: PRINT CHR$(176); STRING$(19, 178); CHR$(176)
FOR i% = 4 TO 20
    LOCATE i%, 15
    PRINT CHR$(176); STRING$(19, 178); CHR$(176)
NEXT i%
LOCATE 21, 15: PRINT CHR$(176); STRING$(19, 178); CHR$(176)
LOCATE 22, 15: PRINT STRING$(21, 176)
 
 '���誨 ��� ����⨪�

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
               ' LOCATE 15, 3: PRINT " ����� �� "    '�⫠���
               ' LOCATE 17, 4: PRINT " ������� "     '�⫠���
     '����� ����祪
 COLOR 11, 1
 LOCATE 4, 17     '������ ��ப�
    PRINT CHR$(201);
        FOR i% = 1 TO 8
            PRINT CHR$(205); CHR$(209);
        NEXT i%
    LOCATE 4, 33: PRINT CHR$(187)

 FOR i% = 5 TO 19           '��ப� �।���
    LOCATE i%, 17
    PRINT CHR$(186);        '��ப� � ���⠬� ��� ���⨪��/�������
        FOR j% = 1 TO 8
            PRINT CHR$(32); CHR$(179);
        NEXT j%
    LOCATE i%, 33: PRINT CHR$(186)
    i% = i% + 1
    LOCATE i%, 17           '��ப� ࠧ����⥫쭠�
    PRINT CHR$(199);
        FOR j% = 1 TO 8
            PRINT CHR$(196); CHR$(197);
        NEXT j%
    LOCATE i%, 33: PRINT CHR$(182)
 NEXT i%

 LOCATE 20, 17     '������ ��ப�
    PRINT CHR$(200);
        FOR i% = 1 TO 8
            PRINT CHR$(205); CHR$(207);
        NEXT i%
    LOCATE 20, 33: PRINT CHR$(188)

    '����� ���᪠��� � �������
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
COLOR 15, 2                 '��ࠦ���� ����
LOCATE 5, 8: PRINT sx%
LOCATE 7, 8: PRINT so%

    IF k$ <> "" THEN            'ᮮ�饭�� � �멣���
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
LOCATE 12, 4: PRINT k$
END IF
END SUB

SUB VvodPolz (px%, py%, p$())
        LOCATE 13, 4: PRINT " ��� ��� "
     DO
     kl$ = ""                       '横� ��६�饭�� �����
        DO WHILE kl$ <> CHR$(13)
            kl$ = INKEY$
        LOCATE 3 + px% * 2, 16 + py% * 2, 1, 0, 31
        rig$ = RIGHT$(kl$, 1)
            IF kl$ = CHR$(27) THEN END
            IF rig$ = "H" AND px% <> 1 THEN px% = px% - 1
            IF rig$ = "P" AND px% <> 8 THEN px% = px% + 1
            IF rig$ = "K" AND py% <> 1 THEN py% = py% - 1
            IF rig$ = "M" AND py% <> 8 THEN py% = py% + 1
        LOOP
            '���������� ���न���� � �����
     LOOP UNTIL p$(px%, py%) = " "
     LOCATE 1, 1, 0         '�������� �����
END SUB

