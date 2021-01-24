# Credit-card-fraud-detection

## Zbiór danych
Dane Credit Card Fraud Detection (Kaggle) -- https://www.kaggle.com/mlg-ulb/creditcardfraud

## Cel projektu
Klasyfikacja (K). Konkretyzacja tego zadania wymaga:

1. ustalenia atrybutu dyskretnego reprezentującego pojęcie docelowe,
1. określenia zakresu przygotowania danych (np. przetworzenia do odpowiedniej postaci tabelarycznej, modyfikacji typów/zbiorów wartości atrybutów, eliminacji/naprawy defektów danych, modyfikacji rozkładu klas, losowania podzbiorów danych),
1. wskazania możliwości zdefiniowania nowych atrybutów,
1. wyboru algorytmu selekcji atrybutów,
1. wyboru algorytmów klasyfikacji,
1. wskazania parametrów algorytmów klasyfikacji wymagających strojenia (w tym parametrów umożliwiających zwiększanie wrażliwości na klasy rzadziej występujące, trudniejsze do prawidłowej predykcji lub o wyższych kosztach pomyłek)m
1. ustalenia procedur i kryteriów oceny jakości modeli (z uwzględnieniem rozkładu oraz, tam gdzie to uzasadnione, kosztów pomyłek).

Wybierając zbiór danych do tego zadania należy się upewnić, że występuje w nim co najmniej jeden atrybut dyskretny, który mógłby pełnić rolę interesującego pojęcia docelowego.

## Uruchamianie skryptów oraz generowanie wynikowaego pliku html.

Obliczenia wykonywane na modelach znajdują się w pliku models.r. Skrypty tam zawarte po wykonaniu istotnych obliczeń zapisują je w odpowiednich plikach o rozszerzeniu .Rdata. 
Należy uruchomić cały skrypt models.r przed wygenerowaniem pliku html.

Plik html można wygenerować z pliku notebook.rmd. Poza analizą danych, odczytuje on zapisane dane z plików .Rdata i podsumowuje wyniki.

Plik common.r zawiera funkcje pomocnicze dla obu pozostałych plików.
