---
title:                "Elixir: Використання регулярних виразів"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Чому

Регулярні вирази є потужним інструментом для обробки тексту в програмуванні. Вони дозволяють шукати та заміняти певні шаблони у рядках, що зберігає час та зусилля програмістів.

## Як використовувати

```Elixir
# Приклад використання регулярних виразів для перевірки валідності email-адреси
def check_email(email) do
  case Regex.match?(~r/\A[\w+\-.]+@[a-z\d\-]+(\.[a-z\d\-]+)*\.[a-z]+\z/i, email) do
    true ->
      IO.puts "Email is valid"
    false ->
      IO.puts "Email is not valid"
  end
end

# Виклик функції
check_email("example@email.com") 
# Output: Email is valid
```

Програма перевіряє, чи відповідає переданий email шаблону вище. Це маленький приклад використання регулярних виразів, але їх можна використовувати для багатьох інших завдань, таких як пошук та заміна певних слів або фраз у рядку.

## Глибока занурення

Крім базового використання регулярних виразів, у Elixir є багато корисних функцій і операторів, які можна використовувати для створення більш складних виразів. Для більш детального огляду всіх можливостей дивіться офіційну [документацію по регулярним виразам у Elixir](https://hexdocs.pm/elixir/Regex.html).

## Дивись також

- [Відеоурок про регулярні вирази у Elixir](https://www.youtube.com/watch?v=uMv1fIKGDiw)
- [Туторіал з простими прикладами використання регулярних виразів у Elixir](https://www.tutorialspoint.com/elixir/elixir_regular_expressions.htm)
- [Бібліотека для валідації регулярними виразами - VerEx](https://github.com/VerbalExpressions/elixir-verex)