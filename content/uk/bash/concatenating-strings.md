---
title:                "Bash: З'єднання рядків"
simple_title:         "З'єднання рядків"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Чому 
Перш ніж навчитися з'єднувати рядки в Bash, важливо зрозуміти, що це може бути корисно для вашої програми. Це допоможе вам створювати більш гнучкі та змінні скрипти.

## Як з'єднувати рядки в Bash
З'єднання рядків в Bash дуже просте. Використовуйте оператор "concatenation" "." та зазначте рядки, які ви хочете з'єднати. Наприклад:

```Bash
myName="Sophie"
greeting="Привіт, моє ім'я -"
echo $greeting$myName
```

При виконанні цього скрипту у терміналі буде виведено: "Привіт, моє ім'я - Sophie"

Але якщо ви хочете з'єднати більше двох рядків, ви можете використовувати цикли для ефективнішого коду. Наприклад:

```Bash
names=("Ліза" "Максим" "Олег")
greeting="Привіт, мої імена -"
for name in ${names[@]}
do
    greeting=$greeting$name", "
done
echo $greeting
```

При виконанні цього скрипту у терміналі буде виведено: "Привіт, мої імена - Ліза, Максим, Олег"

Ви також можете використати спеціальний оператор "+=" для з'єднання рядків. Наприклад:

```Bash
greeting="Привіт, мої імена -"
greeting+="Ліза "
greeting+="Максим "
greeting+="Олег"
echo $greeting
```

При виконанні цього скрипту у терміналі буде виведено: "Привіт, мої імена - Ліза Максим Олег"

## Глибший аналіз з'єднання рядків
Важливо пам'ятати, щоб з'єднуючи рядки, ви завжди вставляєте символ між ними, тому додавайте пробіл або будь-який інший символ за потребою.

З'єднання рядків також може бути корисне, якщо вам потрібно створити динамічні повідомлення для користувачів або форматувати дані для виведення.

## Дивіться також
- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/bash.html)
- [Bash Beginners Guide](https://tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Bash Getting Started Guide](https://www.unixlinux.org/ubuntu-linux-command-list-reference-guide/)
- [Advanced Bash-Scripting Guide](http://www.tldp.org/LDP/abs/html/)
- [Ukrainian translation of the Bash Reference Manual](https://gnu.org.ua/software/bash/manual/bash.uk.html) (Український переклад Руководства з баша)