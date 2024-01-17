---
title:                "Konwertowanie daty na ciąg znaków"
html_title:           "Bash: Konwertowanie daty na ciąg znaków"
simple_title:         "Konwertowanie daty na ciąg znaków"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O co chodzi i dlaczego?

Konwersja daty na ciąg znakowy jest powszechnie stosowanym zadaniem w programowaniu. Polega ona na zamianie informacji o dacie (np. 15 marca 2021) na odpowiedni format tekstu (np. "2021-03-15"). Jest to przydatne w wielu aplikacjach, takich jak generowanie raportów czy tworzenie plików z nazwami opartymi na dacie.

## Jak to zrobić?

W Bashu istnieje wiele sposóbów na konwersję daty na ciąg znakowy. Jeden z najprostszych to użycie polecenia `date` z odpowiednimi opcjami. Na przykład, aby uzyskać datę w formacie RRRR-MM-DD, użyjmy `date +"%Y-%m-%d"`. Oto przykładowy kod:

```bash
# Ustawiamy zmienną z datą dzisiejszą
now=$(date +"%Y-%m-%d")
# Wyświetlamy zmienną
echo $now
```

Output:
`2021-03-15`

## Głębszy zanurzenie

Konwersja daty na ciąg znakowy jest nie tylko praktyczna, ale również ma swoją historię. Wczesne systemy operacyjne nie miały wbudowanych narzędzi do manipulacji datami, więc programiści musieli tworzyć własne algorytmy do tego celu. Współcześnie, istnieje wiele innych sposobów na uzyskanie różnych formatów daty w Bashu, np. użycie komendy `strftime` lub użycie znaku `#` do oznaczania daty w ciągu znaków.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej na temat manipulacji datami w Bashu, polecam przeczytać dokumentację oraz przeprowadzić własne eksperymenty. Oto kilka przydatnych źródeł:

- [Oficjalna dokumentacja Bash](https://www.gnu.org/software/bash/manual/)
- [Artykuł na temat konwersji dat w Bashu](https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/)
- [Kurs programowania z Bashem](https://www.codecademy.com/learn/learn-bash)