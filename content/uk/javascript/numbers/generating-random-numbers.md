---
title:                "Генерація випадкових чисел"
date:                  2024-01-27T20:35:05.616189-07:00
model:                 gpt-4-0125-preview
simple_title:         "Генерація випадкових чисел"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Що і чому?

Генерація випадкових чисел у JavaScript — це техніка, яка використовується для створення непередбачуваності в додатках, від ігор, яким потрібна випадкова поведінка ворогів, до алгоритмів безпеки, які потребують криптографічної випадковості. Ця можливість є ключовою для розробки динамічного користувацького досвіду та безпечних застосунків.

## Як:

### Базова генерація випадкових чисел

Найпростіший спосіб згенерувати випадкове число в JavaScript — це використання `Math.random()`. Ця функція повертає плаваюче випадкове число з псевдовипадкового ряду в діапазоні від 0 (включно) до 1 (виключно).

```javascript
let randomNumber = Math.random();
console.log(randomNumber);
```

### Генерація випадкового числа в межах діапазону

Часто вам буде потрібне випадкове ціле число в певному діапазоні. Цього можна досягти шляхом масштабування та округлення результату `Math.random()`.

```javascript
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

console.log(getRandomInt(1, 100));
```

### Криптографічно безпечні випадкові числа

Для додатків, яким потрібен вищий ступінь випадковості (наприклад, криптографічні операції), можна використовувати метод `crypto.getRandomValues()`. Він забезпечує криптографічну випадковість, на відміну від псевдовипадкових чисел, згенерованих за допомогою `Math.random()`.

```javascript
(function generateSecureRandom() {
  let array = new Uint32Array(1);
  window.crypto.getRandomValues(array);
  console.log(array[0]);
})();
```

## Поглиблений огляд

Історично генерація випадкових чисел у JavaScript була повністю залежною від функції `Math.random()`. Хоча вона зручна для більшості повсякденних випадків використання, її алгоритм, зазвичай варіант генератора псевдовипадкових чисел (PRNG) на кшталт Mersenne Twister, не забезпечує криптографічну безпеку.

Введення Web Cryptography API принесло метод `crypto.getRandomValues()`, що пропонує спосіб генерації чисел, які значно менш передбачувані та підходять для застосунків, чутливих до безпеки. Цей метод використовує джерела випадковості базової операційної системи, такі як `/dev/random` на Unix/Linux, які більш надійні та підходять для криптографічних операцій.

Вкрай важливо обрати правильний метод для певного завдання. `Math.random()` достатньо для базових потреб, як-от прості ігри, анімації або будь-які випадки, де якість випадковості не є критичною. Однак для функцій безпеки, як-от маркери скидання пароля або будь-які криптографічні операції, `crypto.getRandomValues()` є кращим вибором завдяки його вищій якості випадковості.

Зауважимо, що `Math.random()` генерує числа з відомим ухилом у більшості реалізацій, що означає, що деякі числа мають більше шансів випасти, ніж інші. Хоча цей ухил мінімальний і часто непомітний для загальних застосунків, він виключає `Math.random()` з використання в будь-якому криптографічному контексті або застосунках, де критично важлива справедливість, як-от онлайн-ігри на гроші.

На закінчення, хоча вбудовані функції JavaScript для генерації випадкових чисел охоплюють широкий спектр потреб, розуміння відмінностей і обмежень кожного методу є суттєвим для їхнього належного застосування.