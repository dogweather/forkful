---
title:    "Arduino: Видалення символів, які відповідають зразку"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Чому

Неірційно, причиною для видалення символів, що відповідають заданому шаблону, може бути необхідність видалити надлишкову інформацію зі стрічки. Наприклад, якщо потрібно видалити всі числа зі стрічки, залишивши лише букви.

## Як

```Arduino
//Присвоєння стрічки з введеними даними
String myString = "Hello123World789";

//Перемінна, що буде містити відформатовану стрічку
String newString = "";

//Прохід циклом по кожному символу в стрічці
for(int i = 0; i < myString.length(); i++){
    char c = myString[i];
    //Перевіряємо, чи це символ є числом
    if(isDigit(c)){
        //Якщо так, не додаємо його до нової стрічки
        //Інакше - додаємо
        continue;
    }
    newString += c;
}

//Виведення відформатованої стрічки
Serial.print(newString);
```

Вивід: HelloWorld

## Глибоке дослідження

Видалення символів, що відповідають заданому шаблону, можна виконати за допомогою регулярних виразів або шаблонного методу. Це дозволить зробити процес більш гнучким та ефективним для різних варіантів видалення символів.

## Дивіться також

- [Регулярні вирази в Arduino](https://www.arduino.cc/reference/en/language/structure/further-syntax/regular-expressions/)
- [Шаблонний метод в Arduino](https://www.arduino.cc/reference/en/language/structure/typedef/)
- [Розділ "Рядки" у документації Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)