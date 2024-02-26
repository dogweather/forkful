---
title:                "Nội suy chuỗi ký tự"
date:                  2024-02-25T17:07:09.517331-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-02-25, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Nội suy chuỗi trong C# cho phép bạn tạo một chuỗi mới bằng cách bao gồm các biểu thức bên trong một chuỗi ký tự, làm cho việc định dạng và nối chuỗi trở nên dễ dàng hơn. Các lập trình viên sử dụng tính năng này để cải thiện tính đọc và bảo trì của mã, đặc biệt là khi xử lý nội dung chuỗi động.

## Làm thế nào:
Trong C#, nội suy chuỗi được ký hiệu bằng dấu đô la (`$`) theo sau là một chuỗi ký tự. Tên biến hoặc biểu thức được đặt trong dấu ngoặc nhọn (`{}`).

```csharp
string name = "Jane";
int age = 28;
string interpolatedString = $"Xin chào, {name}! Bạn {age} tuổi.";
Console.WriteLine(interpolatedString);
// Xuất ra: Xin chào, Jane! Bạn 28 tuổi.
```

Trong một ví dụ phức tạp hơn, bạn có thể thực hiện các phép toán hoặc gọi phương thức bên trong dấu ngoặc nhọn:

```csharp
double price = 19.99;
int quantity = 3;
string orderDetail = $"Tổng giá: {price * quantity:C2}";
Console.WriteLine(orderDetail);
// Xuất ra: Tổng giá: $59.97
```
Định dạng `:C2` bên trong dấu ngoặc nhọn định dạng số thành tiền tệ với hai chữ số thập phân.

Đối với các trường hợp cần định dạng hoặc địa phương hóa nâng cao hơn, bạn có thể cân nhắc sử dụng phương thức `string.Format` hoặc thư viện như Humanizer. Humanizer có thể thao tác và hiển thị chuỗi, ngày, giờ, khoảng thời gian, số và số lượng một cách dễ đọc hơn cho con người. Dưới đây là ví dụ về việc sử dụng Humanizer cho việc thao tác chuỗi phức tạp. Lưu ý rằng Humanizer không phải là một phần của thư viện chuẩn .NET và yêu cầu cài đặt gói NuGet `Humanizer`.

Đầu tiên, cài đặt Humanizer qua NuGet:

```
Install-Package Humanizer
```

Sau đó, bạn có thể sử dụng nó như sau:

```csharp
using Humanizer;

int dayDifference = 5;
string humanized = $"Sự kiện đã diễn ra cách đây {dayDifference} ngày.".Humanize();
Console.WriteLine(humanized);
// Tùy thuộc vào cấu hình và văn hóa, một kết quả có thể: Sự kiện đã diễn ra cách đây 5 ngày.
```

Ví dụ này minh họa cách sử dụng cơ bản. Humanizer hỗ trợ một loạt các chức năng có thể được áp dụng cho chuỗi, ngày, số và hơn thế nữa, giúp các ứng dụng của bạn trở nên dễ tiếp cận và trực quan hơn.
