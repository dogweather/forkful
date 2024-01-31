---
title:                "כתיבת בדיקות"
date:                  2024-01-19
html_title:           "Bash: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"

category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת בדיקות היא תהליך שבו מתכנתים בודקים את הקוד שלהם כדי לוודא שהוא פועל כראוי. הם עושים זאת כדי לתפוס באגים מוקדם, למנוע שגיאות עתידיות, ולשפר את איכות הקוד.

## איך לעשות:
רובי מספקת מספר ספריות לבדיקת קוד. אהובה במיוחד היא RSpec. קודם כל, התקינו את RSpec:

```Ruby
gem install rspec
```

לאחר מכן, צרו קובץ בדיקה:

```Ruby
# calculator_spec.rb
require 'rspec'
require_relative 'calculator'

describe Calculator do
  it "adds two numbers correctly" do
    expect(Calculator.add(5, 3)).to eq(8)
  end
end
```

וקובץ המחלקה:

```Ruby
# calculator.rb
class Calculator
  def self.add(a, b)
    a + b
  end
end
```

הפעלת הבדיקות:

```bash
rspec calculator_spec.rb
```

פלט לדוגמא:

```
.

Finished in 0.00276 seconds (files took 0.10107 seconds to load)
1 example, 0 failures
```

## עיון מעמיק
בדיקת קוד התחילה בשנות ה-70 כדי לתת מענה לצורך של התעשייה באיכות קוד גבוהה. היום, ישנם מגוון גישות וכלים לבדיקת קוד, כגון מיני-טסט (Minitest), קפיבארה (Capybara) לבדיקות פונקציונליות, ו- Cucumber לבדיקת תכונה. יש להבדיל בין בדיקות יחידה, אשר בודקות חלקים קטנים של הקוד, לבין בדיקות אינטגרציה הבודקות תהליכים מלאים או יישומים.

## ראו גם:
- [Minitest Documentation](http://docs.seattlerb.org/minitest/)
