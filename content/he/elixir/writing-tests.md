---
title:                "Elixir: כתיבת בדיקות"
programming_language: "Elixir"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/writing-tests.md"
---

{{< edit_this_page >}}

# למה

ככל שקוד הופך יותר מורכב, השתתפות בכתיבת טסטים היא חובה. טסטים מאפשרים לנו לוודא שקודנו עובד בדיוק כפי שציפינו ומסייעים למנוע באגים נפוצים.

# כיצד לכתוב טסטים באליקסיר

כדי לכתוב טסטים באליקסיר, אנחנו נשתמש בספריית הטסטים המובנית ExUnit. הבאתי כאן דוגמאות לטסטים לפונקציה בספרייה המובנית `List`.

```elixir
defmodule ListTest do
  use ExUnit.Case

  test "returns the length of a list" do
    assert List.length([1,2,3]) == 3
  end

  test "returns the first element" do
    assert List.first([1,2,3]) == 1
  end
end
```

לאחר המכין את הטסטים, נוכל להריץ אותם באמצעות פקודת הטסט `mix test` ולקבל את התוצאות הבאות:

```elixir
Finished in 0.02 seconds
2 tests, 0 failures
```

להלן דוגמאות נוספות לשימוש בפונקציות המובנות של ExUnit כדי לבדוק שוויון של ערכים ובחינת חריגות:

```elixir
assert 1 + 1 == 2
assert_raise ArgumentError, fn -> String.to_integer("hello") end
```

# חקירה מעמיקה

הטסטים מהווים חלק מרכזי בתהליך פיתוח התוכנה. הם מאפשרים לנו לוודא כי הקוד שלנו עומד בכל התנאים ומגלים באופן מועיל תקלות ובאגים. טסטים גם מאפשרים לפתחים לעבוד על פרויקטים גדולים יותר ולנהל את השינויים לקוד ללא פחד מתקלות לא צפויות.

בנוסף, כאשר אנו מכתיבים טסטים, אנו יכולים להיות יצירתיים יותר עם הקוד שלנו ולנסות דברים חדשים בלי לפחד לפגוע בפונקציונליות שלו.

# ראה גם

- [מדריך