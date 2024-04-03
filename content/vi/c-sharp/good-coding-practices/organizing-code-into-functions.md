---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:04.883079-07:00
description: "Chia code th\xE0nh c\xE1c h\xE0m gi\u1ED1ng nh\u01B0 vi\u1EC7c s\u1EAF\
  p x\u1EBFp c\xE1c vi\xEAn LEGO v\xE0o trong c\xE1c h\u1ED9p \u2014 l\xE0m cho vi\u1EC7\
  c t\xECm ki\u1EBFm v\xE0 s\u1EED d\u1EE5ng ch\xFAng tr\u1EDF n\xEAn d\u1EC5 d\xE0\
  ng h\u01A1n. Ch\xFAng ta l\xE0m\u2026"
lastmod: '2024-03-13T22:44:36.664815-06:00'
model: gpt-4-0125-preview
summary: "Chia code th\xE0nh c\xE1c h\xE0m gi\u1ED1ng nh\u01B0 vi\u1EC7c s\u1EAFp\
  \ x\u1EBFp c\xE1c vi\xEAn LEGO v\xE0o trong c\xE1c h\u1ED9p \u2014 l\xE0m cho vi\u1EC7\
  c t\xECm ki\u1EBFm v\xE0 s\u1EED d\u1EE5ng ch\xFAng tr\u1EDF n\xEAn d\u1EC5 d\xE0\
  ng h\u01A1n."
title: "S\u1EAFp x\u1EBFp m\xE3 th\xE0nh c\xE1c h\xE0m"
weight: 18
---

## Làm thế nào:
Hãy tưởng tượng bạn có đoạn code in lời chào nhiều lần. Không có hàm, nó sẽ là một mớ hỗn độn. Có hàm, mọi thứ sẽ gọn gàng.

```C#
// Không có hàm - lặp lại
Console.WriteLine("Hello, Amy!");
Console.WriteLine("Hello, Bob!");
Console.WriteLine("Hello, Charlie!");

// Có hàm - sạch sẽ hơn
void Greet(string name) {
    Console.WriteLine($"Hello, {name}!");
}

Greet("Amy");
Greet("Bob");
Greet("Charlie");
```

Kết quả giống nhau, nhưng phiên bản thứ hai sạch sẽ hơn nhiều.

## Sâu hơn
Ngày xưa, trong ngôn ngữ lập trình hợp ngữ, bạn sẽ nhảy đến các vị trí code khác nhau với GOTO — loạn và khó theo dõi. Hàm là một bước tiến lớn, giống như các ngăn kéo được tổ chức trong hộp đồ nghề. Có các phương án thay thế không? Chắc chắn rồi. Bạn có phương thức, là hàm trong ngữ cảnh của lớp. Rồi đến lambda và hàm nội tuyến cho các nhiệm vụ nhanh chóng, một lần.

Về việc triển khai - những hàm nhỏ, tập trung là vàng. Chúng dễ kiểm tra và gỡ lỗi hơn. Những hàm lớn với nhiều trách nhiệm có thể trở nên quái vật, kiếm được danh hiệu đáng ngờ là "mã spaghetti". Giữ mỗi hàm một công việc; bạn sẽ cảm ơn mình sau này.

## Xem thêm
Để biết thêm về hàm và các phương pháp hay nhất, hãy xem:

- Clean Code của Robert C. Martin: Nguyên tắc để giữ cho hàm của bạn gọn gàng.
- Refactoring của Martin Fowler: Cách cải thiện mã hiện có.
- Hướng dẫn về Phương pháp của Microsoft C#: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/methods
