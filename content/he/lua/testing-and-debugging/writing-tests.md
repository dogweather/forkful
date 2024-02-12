---
title:                "כתיבת בדיקות"
aliases:
- /he/lua/writing-tests/
date:                  2024-02-03T19:32:10.182588-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבת בדיקות"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

כתיבת בדיקות בתכנות כוללת יצירת חלקי קוד קטנים ונפרדים כדי לאמת באופן אוטומטי שחלקים שונים של היישום שלך פועלים כצפוי. עבור תכנתים ב-Lua, הבדיקות מבטיחות אמינות ותומכות בתחזוקה של איכות הקוד, מהירות את תהליך הניפוי שגיאות והופכות את השינויים בבסיס הקוד לבטוחים יותר.

## איך לעשות:

Lua, שהיא שפת תסריט קלת משקל אך חזקה, אינה כוללת מסגרת בדיקות מובנית. עם זאת, ספריות צד שלישי כמו Busted ו-LuaUnit הופכות את הבדיקה ליחסית פשוטה. כאן, נבחן דוגמאות באמצעות שתיהן.

### שימוש ב-Busted

Busted היא מסגרת בדיקות פופולרית ב-Lua המציעה דרך גמישה לכתוב בדיקות. תחילה, התקן את Busted דרך LuaRocks (מנהל החבילות של Lua) עם `luarocks install busted`. לאחר ההתקנה, אפשר לכתוב את הבדיקות שלך. הנה בדיקה פשוטה עבור פונקציה `add` המחברת שני מספרים:

```lua
-- add.lua
local function add(a, b)
  return a + b
end

return add
```

```lua
-- add_spec.lua
local add = require('add')

describe("פונקציית החיבור", function()
  it("צריכה לחבר שני מספרים בצורה נכונה", function()
    assert.are.equal(5, add(2, 3))
  end)
end)
```

כדי להריץ את הבדיקות, בצעו `busted` בטרמינל. פלט לדוגמה עבור בדיקה שעברה יראה כך:

```
●
1 הצלחה / 0 כישלונות / 0 שגיאות / 0 ממתינות : 0.002 שניות
```

### שימוש ב-LuaUnit

LuaUnit היא מסגרת בדיקות נוספת הנכתבת לפי מוסכמות xUnit וקלה להגדרה. התקן את LuaUnit דרך LuaRocks באמצעות `luarocks install luaunit`. הנה איך ניתן לכתוב בדיקה דומה לזו שלמעלה באמצעות LuaUnit:

```lua
-- add.lua נשאר זהה

-- test_add.lua
luaunit = require('luaunit')
local add = require('add')

function testAdd()
  luaunit.assertEquals(add(2, 3), 5)
end

os.exit(luaunit.LuaUnit.run())
```

הפעלת הסקריפט הזה ישירות דרך Lua (`lua test_add.lua`) תפיק פלט כזה:

```
.
הרצת 1 בדיקות ב-0.001 שניות, 1 הצלחה, 0 כישלונות
```

גם Busted וגם LuaUnit מציעות תכונות נרחבות לטיפול במגוון תרחישי בדיקה, כולל מדמה, מרגל ובדיקה אסינכרונית. הבחירה ביניהן תלויה בצרכים הספציפיים של הפרויקט שלך ובהעדפה האישית שלך בנוגע לתחביר ולפונקציונליות.
