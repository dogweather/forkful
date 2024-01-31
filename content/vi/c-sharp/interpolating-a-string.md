---
title:                "Nội suy chuỗi ký tự"
date:                  2024-01-28T22:02:13.433991-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nội suy chuỗi ký tự"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c-sharp/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Nội suy (interpolation) chuỗi cho phép bạn xây dựng chuỗi sử dụng biểu thức được nhúng trực tiếp. Nó giúp mã lệnh dễ đọc và việc định dạng trở nên dễ dàng.

## Cách thực hiện:
```C# 
string name = "Alex";
int age = 29;
string greeting = $"Xin chào, {name}! Bạn {age} tuổi.";
Console.WriteLine(greeting);
```
Đầu ra:
```
Xin chào, Alex! Bạn 29 tuổi.
```

## Đi sâu vào vấn đề
Nội suy chuỗi được giới thiệu trong C# 6, cải thiện khả năng định dạng chuỗi so với phương thức `String.Format` truyền thống. Trong lịch sử, bạn có thể đã nhìn thấy điều gì đó như thế này:

```C# 
string greeting = string.Format("Xin chào, {0}! Bạn {1} tuổi.", name, age);
```

Nội suy trong C# là một cú pháp đơn giản mà trình biên dịch chuyển đổi thành một lời gọi `String.Format`. Nó hoạt động bằng cách phân tích chuỗi nội suy và thay thế các biểu thức được bao trong `{}` với các biểu diễn chuỗi của kết quả của các biểu thức. Ở bên trong, nó sử dụng `StringBuilder`, làm cho nó hiệu quả hơn việc nối chuỗi trong vòng lặp.

Một phương án khác cho nội suy chuỗi là sử dụng toán tử cộng (`+`) để nối, nhưng điều này có thể trở nên khó đọc và cồng kềnh, và thường xuyên dễ mắc lỗi hơn.

```C# 
string greeting = "Xin chào, " + name + "! Bạn " + age + " tuổi.";
```

Xét đến những phương án thay thế này, nội suy chuỗi thường là sự lựa chọn ưu tiên vì sự rõ ràng và hiệu quả của nó trong hầu hết các tình huống.

## Xem thêm
Để biết thêm về việc định dạng chuỗi trong C#, MSDN là bạn đồng hành của bạn:
- [Nội suy chuỗi](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated)
- [String.Format](https://docs.microsoft.com/en-us/dotnet/api/system.string.format?view=net-6.0)
- [StringBuilder](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder?view=net-6.0)
