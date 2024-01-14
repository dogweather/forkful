---
title:                "Ruby: התחלת פרויקט חדש"
simple_title:         "התחלת פרויקט חדש"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/starting-a-new-project.md"
---

{{< edit_this_page >}}

רגע שני, איך עושים פרויקט חדש בשפת רובי לכיתה.

## למה
למה להתעסק עם פרויקט חדש בשפת רובי? למרבה המזל, רובי היא שפת תכנות יעילה ופשוטה ללמידה, ולכן זהו בחירה נהדרת עבור מתחילים. רובי גם מציעה כלים נחמדים כמו פיתוח בכיתה כדי להפוך את הכתיבה של קוד לקלה יותר ומהנה יותר.

## איך לעשות
לתת פרטים לכיתה
```Ruby
class Student
  attr_accessor :name, :age
  def initialize(name, age)
    @name = name
    @age = age
  end
end
```

להוסיף סטודנט חדש לכיתה
```Ruby
class Class
  def add_student(student)
    @students.push(student)
  end
end

student = Student.new("שירה", 12)
my_class = Class.new
my_class.add_student(student)
```

להדפיס רשימת סטודנטים בכיתה
```Ruby
my_class.students.each do |student|
  puts "#{student.name} בן #{student.age} ימים"
end
```

פלט:
```
שירה בן 12 ימים
```

## חיפוש מידע עמוק
לפני התחלת פרויקט חדש בשפת רובי, כדאי ללמוד עוד כמה דברים חשובים כדי להיות מוכנים טוב יותר.

- תחילה, כדאי ללמוד על התחביר והדקדוק של שפת רובי. כדי ללמוד את זה, כדאי לקרוא פרק 1 בהוראות [שתים עשרה דקות ללמוד את רובי](https://www.gitbook.com/book/valeryv/12minutesselftaughtruby/details).

- כמו כן, כדאי ללמוד כיצד לעבוד עם ספריות וכלים נפוצים ברובי, כגון בונדלרים וגיט.

- ולבסוף, כדי להתחיל פרויקט חדש, כדאי להיות מעוניני ומודע למטרות והתקנים שלך. אתה יכול למצוא השראה מפרויקטים אחרים ולהשתמש בהם כמקור מוטיבציה במהלך הפרויקט.

## ראה גם 
- [ש