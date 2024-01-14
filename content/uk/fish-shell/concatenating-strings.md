---
title:                "Fish Shell: Об'єднання рядків"
simple_title:         "Об'єднання рядків"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Чому

Програмування використовує конкатенацію рядків, щоб об'єднати кілька рядків в один, щоб створити більш складні повідомлення або виводи. Це дозволяє програмістам створювати більш динамічні та зрозумілі програми, які можуть працювати з різними типами даних і повідомлень.

## Як

Конкатенація рядків - це процес об'єднання двох або більше рядків в один. У Fish Shell це можна зробити за допомогою оператора "+" або за допомогою команди "string join". Наприклад:

```
Fish Shell string join
```
```
$ string join Hello, "World"
Hello, World
```

За допомогою оператора "+" можна додавати рядки разом. Наприклад:

```
Fish Shell concatenation operator
```
```
$ "Fish Shell " + "is " + "awesome"
Fish Shell is awesome
```

## Глибшого занурення

У Fish Shell конкатенація рядків може бути більш динамічною та потужною, оскільки дозволяє використовувати змінні та умовні оператори. Наприклад, за допомогою оператора "?" можна додавати різні рядки залежно від умови.

```
Fish Shell conditional concatenation
```
```
$ string join This "is" this? "some " + (if test $USER = "admin"; echo "important" else; echo "regular" end) + " message."
This is some important message.
```

Крім того, у Fish Shell є функція "for", яка дозволяє додавати рядки за допомогою циклу. Наприклад, можна створити рядок, який містить кілька копій слова "Hello" за допомогою наступного коду:

```
Fish Shell loop concatenation
```
```
$ for i in (seq 3)
    set msg $msg Hello
end
echo $msg
HelloHelloHello
```

## Дивись Також

- [Документація Fish Shell про конкатенацію рядків](http://fishshell.com/docs/current/cmds/string.html)
- [Розширення мови у Fish Shell](http://fishshell.com/docs/current/tutorial.html#tut_lang)
- [Бібліотека команд для Fish Shell](https://github.com/fish-shell/fish-shell/tree/master/share/functions)