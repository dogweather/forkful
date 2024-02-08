---
title:                "כתיבת בדיקות"
aliases:
- he/elixir/writing-tests.md
date:                  2024-02-03T19:31:07.380096-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבת בדיקות"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת בדיקות ב-Elixir מערבת יצירת סקריפטים אוטומטיים שמאמתים את התנהגות הקוד שלך. מתכנתים עושים זאת כדי להבטיח איכות, למנוע נסיגות, ולקל על שיפוץ הקוד, ובכך הופכים את תהליך הפיתוח לאמין ויעיל יותר.

## איך לעשות:
Elixir משתמשת ב-ExUnit כמערכת הבדיקות המובנית שלה, אשר חזקה במיוחד וקלה לשימוש. להלן דוגמה בסיסית:

1. צור קובץ בדיקה חדש בתיקית `test` של פרויקט Elixir שלך. לדוגמה, אם אתה מבצע בדיקה למודול בשם `MathOperations`, הקובץ שלך יכול להיות `test/math_operations_test.exs`.

```elixir
# test/math_operations_test.exs
defmodule MathOperationsTest do
  use ExUnit.Case

  # זו תיק לבדיקה פשוטה לבדוק את פונקציית החיבור
  test "החיבור של שני מספרים" do
    assert MathOperations.add(1, 2) == 3
  end
end
```

כדי להריץ את הבדיקות שלך, השתמש בפקודה `mix test` בטרמינל שלך. אם פונקציית `MathOperations.add/2` מחברת נכון שני מספרים, תראה פלט דומה ל:

```
..

סיים ב-0.03 שניות
1 בדיקה, 0 כשלונות
```

לבדיקות שמערבות שירותים חיצוניים או API-ים, ייתכן ותרצה להשתמש בספריות מזויפות, כמו `mox`, כדי למנוע התנגשות עם שירותים אמיתיים:

1. הוסף את `mox` לתלות שלך ב-`mix.exs`:

```elixir
defp deps do
  [
    {:mox, "~> 1.0.0", only: :test},
    # תלויות אחרות...
  ]
end
```

2. הגדר מודול מזויף בעזר של הבדיקות שלך (`test/test_helper.exs`):

```elixir
Mox.defmock(HTTPClientMock, for: HTTPClientBehaviour)
```

3. השתמש במזויף במקרה הבדיקה שלך:

```elixir
# test/some_api_client_test.exs
defmodule SomeAPIClientTest do
  use ExUnit.Case
  import Mox

  # זה אומר ל-Mox לוודא שהמזויף הזה נקרא כצפוי
  setup :verify_on_exit!

  test "מקבל נתונים מה-API" do
    # הכן את תגובת המזויף
    expect(HTTPClientMock, :get, fn _url -> {:ok, "תגובה מזויפת"} end)
    
    assert SomeAPIClient.get_data() == "תגובה מזויפת"
  end
end
```

כאשר מריצים את `mix test`, ההתקנה הזו מאפשרת לך לבודד את הבדיקות היחידה שלך מתלות חיצוניות אמיתיות, ולהתמקד בהתנהגות של הקוד שלך. תבנית זו מבטיחה שהבדיקות שלך רצות מהר ונשארות אמינות, ללא תלות בסטטוס של שירותים חיצוניים או בחיבור לאינטרנט.
