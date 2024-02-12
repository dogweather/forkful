---
title:                "כתיבת בדיקות"
aliases: - /he/ruby/writing-tests.md
date:                  2024-02-03T19:32:40.812179-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבת בדיקות"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
בדיקות ברובי מתייחסות לאימות שהקוד שלך מתנהג כצפוי תחת תנאים שונים. תכנתים כותבים בדיקות כדי להבטיח נכונות, למנוע רגרסיות ולהקל על שיפוצים, שואפים ליצירת יישומים חזקים וקלים לתחזוקה.

## כיצד:
רובי מגיעה עם ספרייה מובנית בשם `Test::Unit` לכתיבת בדיקות יחידה, המכסה תרגולי בדיקה במבנים פשוטים. עם זאת, קהילת רובי לעיתים קרובות נוטה לספריות צד שלישי כמו RSpec ו-Minitest בשל הביטוייות והגמישות המוגברות שלהן.

### שימוש ב-`Test::Unit`:
```ruby
require 'test/unit'

class CalculatorTest < Test::Unit::TestCase
  def test_addition
    result = 2 + 2
    assert_equal 4, result
  end
end
```
הרץ את קובץ הבדיקה מהטרמינל, ואתה אמור לקבל פלט המציין הצלחה או כישלון של הבדיקות:
```
Loaded suite test_calculator
Started
.
Finished in 0.001288 seconds.
1 tests, 1 assertions, 0 failures, 0 errors, 0 pendings, 0 omissions, 0 notifications
100% passed
```

### שימוש ב-RSpec:
RSpec הוא פריימוורק פופולרי ל-BDD (Behavior-Driven Development) עבור רובי. התקן את ה-gem עם `gem install rspec`, אז אתחל אותו בפרויקט שלך עם `rspec --init`.

```ruby
# calculator_spec.rb
require_relative '../calculator'

describe Calculator do
  it 'מוסיף בצורה נכונה שני מספרים' do
    expect(Calculator.add(2, 2)).to eq(4)
  end
end
```
הרץ בדיקות עם הפקודה `rspec`. דוגמה לפלט:
```
.

Finished in 0.002 seconds (files took 0.1 seconds to load)
1 example, 0 failures
```

### שימוש ב-Minitest:
Minitest מספקת מערכת בדיקות מלאה שתומכת ב-TDD, BDD, מזיוף ובנצ'מרקינג. התקן אותה עם `gem install minitest` והשתמש כך:

```ruby
# test_calculator.rb
require 'minitest/autorun'
require_relative '../calculator'

class CalculatorTest < Minitest::Test
  def test_addition
    assert_equal 4, Calculator.add(2, 2)
  end
end
```

הרץ את קובץ הבדיקה ישירות או דרך משימת ה-`rake` שהוגדרה עבור minitest. דוגמה לפלט:
```
Run options: --seed 33407

# Running:

.

Finished in 0.001027s, 974.5922 runs/s, 974.5922 assertions/s.
1 runs, 1 assertions, 0 failures, 0 errors, 0 skips
```

על ידי יישום בדיקות בפרויקטים של רובי שלך באמצעות ספריות אלו, אתה מקפיד על התרגולים הטובים ביותר, מה שמוביל לבסיסי קוד יותר אמינים ונתמכים.
