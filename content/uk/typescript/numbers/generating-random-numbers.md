---
title:                "Генерація випадкових чисел"
date:                  2024-01-27T20:35:56.955044-07:00
model:                 gpt-4-0125-preview
simple_title:         "Генерація випадкових чисел"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Що та чому?

Генерація випадкових чисел у TypeScript полягає в створенні непередбачуваних числових значень у вказаному діапазоні. Програмісти використовують ці випадкові цифри для різноманітних цілей, таких як генерація унікальних ідентифікаторів, симуляція даних для тестування або додавання непередбачуваності до ігор та симуляцій.

## Як це робити:

У TypeScript ви можете генерувати випадкові числа за допомогою глобального об'єкта `Math`. Нижче наведено деякі практичні приклади, які демонструють, як створювати випадкові числа для різних потреб.

### Генерація базового випадкового числа

Щоб згенерувати базове випадкове десяткове число між 0 (включно) та 1 (виключно), ви використовуєте `Math.random()`. Це не вимагає жодних додаткових маніпуляцій:

```TypeScript
const randomNumber = Math.random();
console.log(randomNumber);
```

Це може вивести значення, як от `0.8995452185604771`.

### Генерація випадкового цілого числа між двома значеннями

Коли вам потрібне ціле число між двома конкретними значеннями, ви використовуєте як `Math.random()`, так і деяку арифметику:

```TypeScript
function getRandomInt(min: number, max: number): number {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

const randomInt = getRandomInt(1, 10);
console.log(randomInt);
```

Це може вивести ціле число між 1 та 10, наприклад, `7`.

### Генерація унікального ідентифікатора

Випадкові числа можуть бути поєднані з іншими методами для створення унікальних ідентифікаторів, наприклад, простий фрагмент генератора UUID:

```TypeScript
function generateUUID(): string {
    return 'xxxxyxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
        const r = Math.random() * 16 | 0, v = c == 'x' ? r : (r & 0x3 | 0x8);
        return v.toString(16);
    });
}

const uuid = generateUUID();
console.log(uuid);
```

Це генерує рядок, який нагадує UUID, наприклад, `110e8400-e29b-41d4-a716-446655440000`.

## Поглиблено

Основний метод для генерації випадкових чисел у JavaScript, а отже, і в TypeScript, `Math.random()`, покладається на генератор псевдовипадкових чисел (PRNG). Важливо зауважити, що, хоча результати можуть видатися випадковими, вони генеруються детерміністичним алгоритмом на основі початкового значення “сім’ї”. Тому числа, створені за допомогою `Math.random()`, не є справді випадковими і не повинні використовуватися для криптографічних цілей.

Для криптографічно безпечних випадкових чисел Web Crypto API пропонує `crypto.getRandomValues()`, який доступний в середовищах, що підтримують стандарт Web Crypto, включно з сучасними браузерами та Node.js (через модуль `crypto`). Ось швидкий приклад, який ілюструє його використання у TypeScript для генерації безпечного випадкового числа в діапазоні:

```TypeScript
function secureRandom(min: number, max: number): number {
    const array = new Uint32Array(1);
    window.crypto.getRandomValues(array);
    return min + (array[0] % (max - min + 1));
}

const secureRandNum = secureRandom(1, 100);
console.log(secureRandNum);
```

Цей метод забезпечує більш сильний рівень випадковості і більше підходить для застосувань, чутливих до безпеки. Однак, він також більш ресурсоємний і може бути не потрібний для більш простих задач, як-от прості симуляції або генерація некритичних випадкових значень.