---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:57.648476-06:00
description: "\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E42\u0E04\u0E23\u0E07\u0E2A\
  \u0E23\u0E49\u0E32\u0E07\u0E42\u0E04\u0E49\u0E14 (Refactoring) \u0E40\u0E1B\u0E47\
  \u0E19\u0E01\u0E23\u0E30\u0E1A\u0E27\u0E19\u0E01\u0E32\u0E23\u0E02\u0E2D\u0E07\u0E01\
  \u0E32\u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\
  \u0E07\u0E42\u0E04\u0E49\u0E14\u0E04\u0E2D\u0E21\u0E1E\u0E34\u0E27\u0E40\u0E15\u0E2D\
  \u0E23\u0E4C\u0E17\u0E35\u0E48\u0E21\u0E35\u0E2D\u0E22\u0E39\u0E48\u0E42\u0E14\u0E22\
  \u0E44\u0E21\u0E48\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E41\u0E1B\u0E25\u0E07\
  \u0E1E\u0E24\u0E15\u0E34\u0E01\u0E23\u0E23\u0E21\u0E20\u0E32\u0E22\u0E19\u0E2D\u0E01\
  \u0E02\u0E2D\u0E07\u0E21\u0E31\u0E19\u2026"
lastmod: '2024-03-17T21:57:56.234944-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E42\u0E04\u0E23\u0E07\u0E2A\
  \u0E23\u0E49\u0E32\u0E07\u0E42\u0E04\u0E49\u0E14 (Refactoring) \u0E40\u0E1B\u0E47\
  \u0E19\u0E01\u0E23\u0E30\u0E1A\u0E27\u0E19\u0E01\u0E32\u0E23\u0E02\u0E2D\u0E07\u0E01\
  \u0E32\u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\
  \u0E07\u0E42\u0E04\u0E49\u0E14\u0E04\u0E2D\u0E21\u0E1E\u0E34\u0E27\u0E40\u0E15\u0E2D\
  \u0E23\u0E4C\u0E17\u0E35\u0E48\u0E21\u0E35\u0E2D\u0E22\u0E39\u0E48\u0E42\u0E14\u0E22\
  \u0E44\u0E21\u0E48\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E41\u0E1B\u0E25\u0E07\
  \u0E1E\u0E24\u0E15\u0E34\u0E01\u0E23\u0E23\u0E21\u0E20\u0E32\u0E22\u0E19\u0E2D\u0E01\
  \u0E02\u0E2D\u0E07\u0E21\u0E31\u0E19\u2026"
title: "\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\
  \u0E49\u0E32\u0E07\u0E42\u0E04\u0E49\u0E14"
weight: 19
---

## อะไร & ทำไม?

การปรับโครงสร้างโค้ด (Refactoring) เป็นกระบวนการของการปรับโครงสร้างโค้ดคอมพิวเตอร์ที่มีอยู่โดยไม่เปลี่ยนแปลงพฤติกรรมภายนอกของมัน โปรแกรมเมอร์ทำการนี้เพื่อทำความสะอาดโค้ด, เพิ่มความสามารถในการอ่าน, ลดความซับซ้อน, และปรับปรุงความสามารถในการบำรุงรักษา

## วิธีทำ:

มาปรับโครงสร้างเมทอดง่ายๆใน C# ที่คำนวณและพิมพ์ผลรวมของอาร์เรย์ของตัวเลขกัน:

ก่อนปรับโครงสร้าง:
```C#
public class Calculator
{
    public void CalculateSum()
    {
        int[] numbers = { 1, 2, 3, 4, 5 };
        int sum = 0;
        for (int i = 0; i < numbers.Length; i++)
        {
            sum += numbers[i];
        }
        Console.WriteLine("The sum is " + sum);
    }
}
```

หลังจากปรับโครงสร้าง:
```C#
public class Calculator
{
    private readonly int[] _numbers;

    public Calculator(int[] numbers)
    {
        _numbers = numbers;
    }

    public int CalculateSum()
    {
        return _numbers.Sum();
    }

    public void DisplaySum()
    {
        Console.WriteLine($"The sum is {CalculateSum()}");
    }
}

// การใช้งาน:
var calculator = new Calculator(new[] { 1, 2, 3, 4, 5 });
calculator.DisplaySum();
```

ด้วยการปรับโครงสร้าง, เราได้แยกกังวลออกจากกัน, ทำให้คลาส `Calculator` มีความยืดหยุ่นมากขึ้นโดยอนุญาตให้รับอาร์เรย์ของตัวเลขใดๆ, และใช้ประโยชน์จาก LINQ เพื่อทำให้การคำนวณผลรวมง่ายขึ้น

## การศึกษาลึก

การปรับโครงสร้างมีรากฐานมาจากชุมชนโปรแกรมเมอร์ตัวเล็ก (smalltalk) และได้รับความนิยมในช่วงทศวรรษที่ 1990 โดยหนังสือ "Refactoring: Improving the Design of Existing Code" ของมาร์ติน ฟาวเลอร์ ตลอดหลายปีที่ผ่านมา, มันได้กลายเป็นส่วนหนึ่งของวิธีการ agile และปฏิบัติการเขียนโค้ดที่ดี

มีวิธีการปรับโครงสร้างที่หลากหลาย, เช่น Red-Green-Refactor ใน Test-Driven Development (TDD) ประกันว่าการปรับโครงสร้างจะไม่นำเข้าบั๊กด้วยการเริ่มต้นด้วยการทดสอบที่ล้มเหลว, ทำให้มันผ่าน, แล้วทำความสะอาดโค้ด

เมื่อดำเนินการปรับโครงสร้าง, สิ่งสำคัญคือต้องมีชุดการทดสอบที่ครอบคลุมเพื่อให้แน่ใจว่าไม่มีการทำลายฟังก์ชันการทำงานระหว่างกระบวนการ โปรแกรมช่วยการปรับโครงสร้างอัตโนมัติ, เช่น ReSharper สำหรับ C#, ก็สามารถช่วยในกระบวนการนี้ได้โดยการให้วิธีที่ปลอดภัยในการเปลี่ยนโครงสร้างของโค้ด อย่างไรก็ตาม, เครื่องมือควรเป็นเพียงส่วนเสริมกับความเข้าใจอย่างลึกซึ้งเกี่ยวกับฐานโค้ดและหลักการเขียนโค้ด

## ดูเพิ่มเติม

- งานชิ้นสำคัญของมาร์ติน ฟาวเลอร์เกี่ยวกับการปรับโครงสร้าง: [Refactoring: Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
- คู่มือของไมโครซอฟต์เกี่ยวกับการปรับโครงสร้างใน Visual Studio: [Refactoring (C#)](https://docs.microsoft.com/en-us/visualstudio/ide/refactoring-in-visual-studio?view=vs-2022)
- การศึกษาอย่างละเอียดเกี่ยวกับรูปแบบการปรับโครงสร้างพร้อมตัวอย่าง: [SourceMaking Refactoring](https://sourcemaking.com/refactoring)
