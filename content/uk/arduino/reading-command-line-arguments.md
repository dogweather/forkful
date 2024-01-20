---
title:                "Читання аргументів командного рядка"
html_title:           "Arduino: Читання аргументів командного рядка"
simple_title:         "Читання аргументів командного рядка"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Що і навіщо?
Читання аргументів командного рядка - це процес, в якому ваша програма приймає вхідні дані безпосередньо під час запуску. Використання аргументів командного рядка дає можливість зробити ваш код більш гнучким і адаптивним до різних ситуацій.

## Як це робити:
В Arduino читання аргументів командного рядка може виглядати як-ось так:

```Arduino
void setup() {
  Console.begin(); 

  while(!Console);
  
  Console.println("Number of Arguments: " + String(argc)); 
  for(int i = 0; i < argc; i++) Console.println("Arg[" + String(i) + "]: " + String(argv[i])); 
}

void loop() {}

```

Вихідні дані зразка можуть виглядати отак:

``` 
Number of Arguments: 2
Arg[0]: /path_to_your_program
Arg[1]: your_argument
```

## Поглиблений огляд
Читання аргументів командного рядка є загальноприйнятою практикою, яка впроваджена ще з часів, коли більшість програм були консольними. Альтернативою є використання файлів конфігурації або GUI для надання вхідних даних. Якщо говорити про реалізацію читання командних аргументів в Arduino, її можна виконати через надзвичайно важливий набір функцій setup() та loop(). У setup() обробляються вхідні дані, а в loop() - все інше.

## Дивіться також
Знайдіть додаткову інформацію про працю з командним рядком в Arduino за наступними посиланнями:

- [Arduino Command Line Interface](https://arduino.github.io/arduino-cli/latest/)
- [Working with Command Line Arguments on Arduino](https://learn.sparkfun.com/tutorials/command-line-esp8266-ota/)