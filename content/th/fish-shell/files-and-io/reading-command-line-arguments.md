---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:44.296016-06:00
description: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\
  \u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E1A\u0E19\u0E1A\u0E23\u0E23\u0E17\u0E31\
  \u0E14\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07 (command line arguments) \u0E40\u0E1B\
  \u0E47\u0E19\u0E01\u0E32\u0E23\u0E08\u0E31\u0E1A\u0E04\u0E39\u0E48\u0E02\u0E49\u0E2D\
  \u0E21\u0E39\u0E25\u0E40\u0E1E\u0E34\u0E48\u0E21\u0E40\u0E15\u0E34\u0E21\u0E17\u0E35\
  \u0E48\u0E04\u0E38\u0E13\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E2B\u0E25\u0E31\u0E07\u0E08\
  \u0E32\u0E01\u0E0A\u0E37\u0E48\u0E2D\u0E2A\u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C\u0E02\
  \u0E2D\u0E07\u0E04\u0E38\u0E13,\u2026"
lastmod: '2024-03-17T21:57:56.660046-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\
  \u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E1A\u0E19\u0E1A\u0E23\u0E23\u0E17\u0E31\
  \u0E14\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07 (command line arguments) \u0E40\u0E1B\
  \u0E47\u0E19\u0E01\u0E32\u0E23\u0E08\u0E31\u0E1A\u0E04\u0E39\u0E48\u0E02\u0E49\u0E2D\
  \u0E21\u0E39\u0E25\u0E40\u0E1E\u0E34\u0E48\u0E21\u0E40\u0E15\u0E34\u0E21\u0E17\u0E35\
  \u0E48\u0E04\u0E38\u0E13\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E2B\u0E25\u0E31\u0E07\u0E08\
  \u0E32\u0E01\u0E0A\u0E37\u0E48\u0E2D\u0E2A\u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C\u0E02\
  \u0E2D\u0E07\u0E04\u0E38\u0E13,\u2026"
title: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\u0E34\
  \u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E08\u0E32\u0E01\u0E04\u0E33\u0E2A\u0E31\u0E48\
  \u0E07\u0E25\u0E33\u0E14\u0E31\u0E1A"
---

{{< edit_this_page >}}

## อะไร & ทำไม?
การอ่านอาร์กิวเมนต์บนบรรทัดคำสั่ง (command line arguments) เป็นการจับคู่ข้อมูลเพิ่มเติมที่คุณพิมพ์หลังจากชื่อสคริปต์ของคุณ, เหมือนกับการทำความลับเพื่อปรับแต่งพฤติกรรมของสคริปต์ นักพัฒนาทำเช่นนี้เพื่อทำให้สคริปต์มีความยืดหยุ่นและมีปฏิสัมพันธ์ได้โดยไม่ยุ่งยาก

## วิธีทำ:

สมมติว่า `greet.fish` เป็นสคริปต์ของคุณ คุณต้องการให้มันรับชื่อและส่งคืนคำทักทาย

```fish
#!/usr/bin/env fish

# อาร์กิวเมนต์ถูกจัดเก็บใน $argv
# $argv[1] เป็นอาร์กิวเมนต์ที่หนึ่ง, $argv[2] เป็นอาร์กิวเมนต์ที่สอง, และอื่น ๆ

set name $argv[1]
echo "Hello, $name!"
```

ทดสอบการรัน:

```shell
$ fish greet.fish World
Hello, World!
```

ตอนนี้, ด้วยอาร์กิวเมนต์หลายตัว:

```fish
#!/usr/bin/env fish

# วนซ้ำผ่านอาร์กิวเมนต์ทั้งหมด
for arg in $argv
    echo "Hello, $arg!"
end
```

ลองทดสอบ:

```shell
$ fish greet.fish Earth Mars Venus
Hello, Earth!
Hello, Mars!
Hello, Venus!
```

การจัดการกับธง (เช่น `-u` สำหรับตัวพิมพ์ใหญ่):

```fish
#!/usr/bin/env fish

# ตรวจสอบอาร์กิวเมนต์ "-u"
set -l uppercase_mode off
for arg in $argv
    if test "$arg" = "-u"
        set uppercase_mode on
    else if set -q uppercase_mode[1]; and string match --quiet -- "$uppercase_mode" "on"
        echo (string upper "$arg")
    else
        echo $arg
    end
end
```

และการเรียกใช้:

```shell
$ fish greet.fish -u mercury venus
MERCURY
VENUS
```

## การศึกษารายละเอียด

Fish Shell มีความสามารถในการอ่านอาร์กิวเมนต์บนบรรทัดคำสั่งมาเป็นเวลานานแล้ว, คล้ายกับ shell อื่นๆ สิ่งที่ทำให้ Fish แตกต่างคือความเรียบง่ายในการออกแบบ ไม่มี `$1, $2... $n` ให้ต้องจำ; มันเป็นอาร์เรย์ `$argv`, ถือเป็นพื้นที่คุ้นเคยหากคุณเคยมีประสบการณ์กับภาษาโปรแกรมอื่น

มีทางเลือกอื่นๆ เช่น bash, zsh ฯลฯ, แต่ไวยากรณ์สคริปต์ของ Fish มุ่งเน้นไปที่ความสามารถในการอ่านและชัดเจน แตกต่างจากการใช้คำสั่ง `shift` แบบดั้งเดิมหรือการจัดการกับ `$@` สำหรับอาร์กิวเมนต์ทั้งหมด Fish มี `$argv` และโครงสร้างสคริปต์ที่น่ารักเช่นลูป `for` และเงื่อนไข `if` ที่น้อยเกี่ยวกับสัญลักษณ์ลึกลับแต่มากกว่าในเรื่องของคำที่ชัดเจน

เมื่อดำเนินการ, มันสำคัญที่จะพิจารณาว่าสคริปต์ของคุณจะถูกใช้อย่างไร มันจะต้องการค่าเริ่มต้นหรือไม่? ผู้ใช้จะรู้ว่าต้องป้อนอะไรหรือไม่? ตรวจสอบให้มั่นใจว่าคุณจัดการกับกรณีที่ผู้ใช้ลืมใส่อาร์กิวเมนต์หรือใส่ผิดลำดับ

## ดูเพิ่มเติม

- เอกสารการใช้งานอย่างเป็นทางการของ Fish ที่อธิบายเกี่ยวกับอาร์กิวเมนต์บนบรรทัดคำสั่ง: [fishshell.com/docs/current/#syntax-command-line](https://fishshell.com/docs/current/#syntax-command-line)
- สำหรับการเขียนสคริปต์ขั้นสูงและสร้างฟังก์ชันของคุณเองใน Fish: [fishshell.com/docs/current/#defining-functions](https://fishshell.com/docs/current/#defining-functions)
- บทนำสำหรับผู้ใช้ Fish ที่มีพื้นหลังจาก shell อื่น: [fishshell.com/docs/current/tutorial.html](https://fishshell.com/docs/current/tutorial.html)
