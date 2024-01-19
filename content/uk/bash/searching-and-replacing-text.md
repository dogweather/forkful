---
title:                "Пошук та заміна тексту"
html_title:           "C++: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Що та чому?

Пошук та заміна тексту - це процес виявлення вказаних символьних послідовностей та заміна їх на інші. Програмісти роблять це автоматизовано, щоб керувати великими наборами даних ефективніше.

## Як це робити:

Ось декілька прикладів коду:

```Bash 
# Заміна тексту в файлі за допомогою sed
sed 's/foo/bar/g' filename 

# Заміна тексту в "на місці" з резервною копією файлу за допомогою sed
sed -i.bak 's/foo/bar/g' filename 

# Заміна тексту в рядку за допомогою awk
echo 'Hello foo' | awk '{gsub(/foo/,"bar"); print}'
```
Вихідний код:
  
```Bash
Hello bar
```

## Більш глибоке занурення:

Bash використовує різні утиліти, такі як sed та awk, для пошуку та заміни тексту вихідно з Unix-а. Є також різні способи виконання цього завдання, залежно від потреб, включаючи використання grep та Perl.

Використання sed та awk зазнає певних обмежень у складних ситуаціях, оскільки вони працюють лінійно і обробляють кожен рядок окремо. 

## Далі читайте:

Взаємодія з текстовими стрічками є ключовою частиною програмування. Ось дорожня карта для подальшого вивчення цієї теми:

- GNU Sed Manual: [https://www.gnu.org/software/sed/manual/sed.html](https://www.gnu.org/software/sed/manual/sed.html)
- GNU Awk User's Guide: [https://www.gnu.org/software/gawk/manual/](https://www.gnu.org/software/gawk/manual/)
- Bash String Manipulation Guide: [https://tldp.org/LDP/abs/html/string-manipulation.html](https://tldp.org/LDP/abs/html/string-manipulation.html)