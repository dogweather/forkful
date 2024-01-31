---
title:                "Перетворення рядка на великі літери"
date:                  2024-01-19
html_title:           "Arduino: Перетворення рядка на великі літери"
simple_title:         "Перетворення рядка на великі літери"

category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Що і Чому?
Коли ми кажемо "зробити рядок з великої літери", маємо на увазі зміну першої букви тексту на велику. Програмісти це роблять, щоб виділити слова або дотримуватися певних стандартів введення тексту.

## Як це зробити:
```Ruby
# Варіант 1: capitalize
name = "василь"
puts name.capitalize  # Виводить "Василь"

# Варіант 2: З великої літери всі слова в реченні
phrase = "слава україні!"
puts phrase.split.map(&:capitalize).join(' ')  # Виводить "Слава Україні!"

# Варіант 3: Unicode-свідомий спосіб (враховуючи мовні особливості)
require 'active_support/core_ext/string'

locale_string = "щасливого ранку"
puts locale_string.mb_chars.capitalize.to_s  # Виводить "Щасливого ранку"
```

## Поглиблений Занурення:
Сам метод `capitalize` в Ruby з'явився давно, щоб допомогти стандартизувати рядки. Метод простий: він перетворює перший символ рядка у великий регістр, а решту — у маленький. Але є нюанси, особливо з не-англійськими мовами через їхній осібний Unicode склад. Альтернативи? Active Support (Rails) надає методи, які краще обходяться з Unicode, і забезпечують більше функціональности (зокрема, `mb_chars`). Щодо впровадження, Ruby використовує UTF-8 за замовчуванням, тож краще підтримує різноманітність символів і регіонів.

## Посилання для Ознайомлення:
- [Ruby's String Documentation](https://ruby-doc.org/core-3.1.0/String.html)
- [Rails' Active Support Core Extensions](https://api.rubyonrails.org/classes/String.html)
- [Керівництво по стилю Ruby](https://github.com/rubocop/ruby-style-guide)
