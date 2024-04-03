---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:26.927554-07:00
description: "C\xE1ch th\u1EE9c: Trong C#, b\u1EA1n l\xE0m vi\u1EC7c v\u1EDBi m\u1EA3\
  ng k\u1EBFt h\u1EE3p s\u1EED d\u1EE5ng l\u1EDBp `Dictionary<TKey, TValue>`. D\u01B0\
  \u1EDBi \u0111\xE2y l\xE0 m\u1ED9t v\xED d\u1EE5 nhanh \u0111\u1EC3 b\u1EA1n b\u1EAF\
  t \u0111\u1EA7u."
lastmod: '2024-03-13T22:44:36.647895-06:00'
model: gpt-4-0125-preview
summary: "Trong C#, b\u1EA1n l\xE0m vi\u1EC7c v\u1EDBi m\u1EA3ng k\u1EBFt h\u1EE3\
  p s\u1EED d\u1EE5ng l\u1EDBp `Dictionary<TKey, TValue>`."
title: "S\u1EED d\u1EE5ng m\u1EA3ng li\xEAn k\u1EBFt"
weight: 15
---

## Cách thức:
Trong C#, bạn làm việc với mảng kết hợp sử dụng lớp `Dictionary<TKey, TValue>`. Dưới đây là một ví dụ nhanh để bạn bắt đầu:

```C#
using System;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        // Tạo một từ điển
        Dictionary<string, int> fruitBasket = new Dictionary<string, int>();

        // Thêm các cặp khoá-giá trị
        fruitBasket.Add("Apples", 5);
        fruitBasket.Add("Oranges", 10);

        // Truy cập giá trị bằng khoá của nó
        Console.WriteLine("Apples: " + fruitBasket["Apples"]);
        
        // Cập nhật một giá trị
        fruitBasket["Apples"] = 7;
        Console.WriteLine("Apples cập nhật: " + fruitBasket["Apples"]);
        
        // Xóa một cặp khoá-giá trị
        fruitBasket.Remove("Oranges");

        // Duyệt qua từ điển
        foreach (var pair in fruitBasket)
        {
            Console.WriteLine(pair.Key + ": " + pair.Value);
        }
    }
}
```
Kết quả mẫu:
```
Apples: 5
Apples cập nhật: 7
Apples: 7
```

Ví dụ này trình bày cách tạo một từ điển, thêm, truy cập, cập nhật, xóa các phần tử và duyệt qua nó.

## Sâu hơn
Khái niệm về mảng kết hợp trở lại từ việc sử dụng chúng trong các ngôn ngữ kịch bản như Perl và PHP, nơi chúng cung cấp sự linh hoạt trong quản lý bộ sưu tập dữ liệu. Trong C#, `Dictionary<TKey, TValue>` là triển khai mặc định, được giới thiệu trong .NET Framework 2.0. Nó lưu trữ dữ liệu trong một bảng băm, đảm bảo hiệu suất tìm kiếm, thêm và xóa hiệu quả.

Tuy nhiên, đáng chú ý là mặc dù từ điển rất linh hoạt, chúng không phải lúc nào cũng là lựa chọn tốt nhất của bạn. Để duy trì bộ sưu tập có thứ tự, bạn có thể xem xét `SortedDictionary<TKey, TValue>` hoặc `SortedList<TKey, TValue>`, chúng cung cấp thứ tự được sắp xếp nhưng tốn kém hơn về thao tác chèn và xóa. Đối với các kịch bản đòi hỏi tính an toàn với các luồng, `ConcurrentDictionary<TKey, TValue>` thêm gánh nặng nhưng đảm bảo truy cập an toàn từ nhiều luồng mà không cần khoá thủ công.

Cuối cùng, sự lựa chọn triển khai mảng kết hợp trong C# phụ thuộc vào nhu cầu cụ thể của bạn về thứ tự, hiệu suất và tính an toàn luồng.
