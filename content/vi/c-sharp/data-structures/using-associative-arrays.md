---
title:                "Sử dụng mảng liên kết"
aliases:
- /vi/c-sharp/using-associative-arrays/
date:                  2024-01-30T19:10:26.927554-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng mảng liên kết"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c-sharp/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Mảng kết hợp, hay từ điển trong C#, cho phép bạn lưu trữ và quản lý các cặp khoá và giá trị. Chúng là lựa chọn hàng đầu khi bạn cần lấy nhanh giá trị dựa trên một bộ nhận diện duy nhất, làm cho việc quản lý dữ liệu trở nên dễ dàng trong các ứng dụng phức tạp.

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
