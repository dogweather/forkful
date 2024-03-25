---
title:                "การทำให้ตัวอักษรแรกในสตริงเป็นตัวพิมพ์ใหญ่"
date:                  2024-03-25T17:32:16.556755-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-25, dogweather, edited and tested
  - 2024-03-25, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## อะไร & ทำไม?
การทำให้สตริงเป็นตัวพิมพ์ใหญ่โดยปกติหมายถึงการเปลี่ยนอักขระตัวแรกของสตริงให้เป็นตัวพิมพ์ใหญ่และเปลี่ยนส่วนที่เหลือเป็นตัวพิมพ์เล็ก แต่บางครั้งมันอาจหมายถึงเพียงแค่การแน่ใจว่าอักขระตัวแรกเป็นตัวพิมพ์ใหญ่โดยที่เหลือสตริงเหมือนเดิม ตามความเห็นของฉัน มันเป็นคำที่ค่อนข้างกว้าง

## วิธีการ:
Ruby มี [วิธีการจัดการสตริงที่ตรงไปตรงมา](https://docs.ruby-lang.org/en/3.3/String.html) รวมถึงการทำให้เป็นตัวพิมพ์ใหญ่:

```ruby
# วิธีการในตัวของ Ruby
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

มีประโยชน์มาก

วิธีการ `.capitalize` ของ Ruby สะดวก แต่มันจะทำให้ตัวอักษรตัวแรกเป็นตัวพิมพ์ใหญ่เท่านั้น หากต้องการควบคุมมากขึ้นหรือทำให้ทุกคำในสตริงเป็นตัวพิมพ์ใหญ่ (ที่รู้จักกันว่าคำแต่ละคำให้เป็นตัวพิมพ์ใหญ่), คุณอาจต้องการใช้วิธีการ `titleize` จากส่วนขยาย Rails ActiveSupport หรือทำเอง:

```ruby
# การใช้ 'titleize' ของ ActiveSupport ใน Rails
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# การแก้ไขด้วยตนเอง
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

วิธีนี้จะแยกสตริงออกเป็นอาร์เรย์ของคำ ทำให้แต่ละคำเป็นตัวพิมพ์ใหญ่ แล้วรวมมันกลับมาด้วยกันด้วยการใส่ช่องว่างแต่ละคำ

ส่วนตัวแล้ว ฉันนำไอเดียนี้ไปใช้ไกลกว่านี้ในโค้ดของฉัน ฉันเขียนวิธีการ [`titleize` ของตัวเองที่คำนึงถึงคำเล็กๆ เช่น "a" และ "the"](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
