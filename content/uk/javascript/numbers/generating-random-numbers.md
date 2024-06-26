---
date: 2024-01-27 20:35:05.616189-07:00
description: "\u042F\u043A: \u041D\u0430\u0439\u043F\u0440\u043E\u0441\u0442\u0456\
  \u0448\u0438\u0439 \u0441\u043F\u043E\u0441\u0456\u0431 \u0437\u0433\u0435\u043D\
  \u0435\u0440\u0443\u0432\u0430\u0442\u0438 \u0432\u0438\u043F\u0430\u0434\u043A\u043E\
  \u0432\u0435 \u0447\u0438\u0441\u043B\u043E \u0432 JavaScript \u2014 \u0446\u0435\
  \ \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F `Math.random()`.\
  \ \u0426\u044F \u0444\u0443\u043D\u043A\u0446\u0456\u044F \u043F\u043E\u0432\u0435\
  \u0440\u0442\u0430\u0454 \u043F\u043B\u0430\u0432\u0430\u044E\u0447\u0435 \u0432\
  \u0438\u043F\u0430\u0434\u043A\u043E\u0432\u0435 \u0447\u0438\u0441\u043B\u043E\
  \ \u0437\u2026"
lastmod: '2024-03-13T22:44:49.986756-06:00'
model: gpt-4-0125-preview
summary: "\u041D\u0430\u0439\u043F\u0440\u043E\u0441\u0442\u0456\u0448\u0438\u0439\
  \ \u0441\u043F\u043E\u0441\u0456\u0431 \u0437\u0433\u0435\u043D\u0435\u0440\u0443\
  \u0432\u0430\u0442\u0438 \u0432\u0438\u043F\u0430\u0434\u043A\u043E\u0432\u0435\
  \ \u0447\u0438\u0441\u043B\u043E \u0432 JavaScript \u2014 \u0446\u0435 \u0432\u0438\
  \u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F `Math.random()`."
title: "\u0413\u0435\u043D\u0435\u0440\u0430\u0446\u0456\u044F \u0432\u0438\u043F\u0430\
  \u0434\u043A\u043E\u0432\u0438\u0445 \u0447\u0438\u0441\u0435\u043B"
weight: 12
---

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
