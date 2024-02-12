---
title:                "התחלת פרויקט חדש"
aliases:
- /he/ruby/starting-a-new-project.md
date:                  2024-01-20T18:04:34.997528-07:00
model:                 gpt-4-1106-preview
simple_title:         "התחלת פרויקט חדש"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/starting-a-new-project.md"
---

{{< edit_this_page >}}

## מה ולמה?
כשאנחנו מתחילים פרויקט חדש ברובי, אנחנו למעשה יוצרים אוסף של קבצים ותיקיות שישמשו כבסיס לקוד שלנו. תכניתנים עושים זאת כדי לסדר את הקוד שלהם בצורה מובנית – בין אם זה סקריפט קטן או אפליקציה מורכבת.

## איך לעשות:
אתה יכול להשתמש בכלי כמו `rails new` או `bundle gem` ליצירת פרויקט חדש בצורה מסודרת:

```Ruby
# יצירת פרויקט חדש בדרך של Rails
rails new my_new_project

# יצירת ג'מ (gem) חדש עם Bundler
bundle gem my_new_gem
```

כאשר אתה מריץ את הפקודות הללו, רובי יצור תיקיות וקבצים הנחוצים להתחלת העבודה על הפרויקט שלך.

## עיון מעמיק
ליצירת פרויקט ברובי היו השפעות מגוונות לאורך השנים. 'Bundler' ו-'Rails' הם שני כלים ששינו את אופן יצירת וניהול פרויקטים ברובי. 'Rails' התפרסם בשל כוחו לקחת חבילות (gems) וליצור מערכת שלמה, בעוד 'Bundler' מסייע בניהול תלותיות בפרויקט. על אף שישנן חלופות כמו 'Sinatra' ו-'Hanami' ל-Rails, ו-'Rake' ו-'Thor' ל-Bundler, הם נותרו הבחירה הפופולרית בקרב רבים מפיתחי ה-Ruby.

## ראה גם
- [Ruby on Rails Guides](https://guides.rubyonrails.org/)
- [Bundler's Documentation](https://bundler.io/docs.html)
- [Creating a New Gem](https://guides.rubygems.org/make-your-own-gem/)
- [Learn Ruby the Hard Way - More on Gems](https://learnrubythehardway.org/book/ex46.html)
- [Sinatra](http://sinatrarb.com/)
- [Hanami](https://hanamirb.org/)
