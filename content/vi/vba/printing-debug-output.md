---
title:                "In ra thông tin debug"
aliases:
- vi/vba/printing-debug-output.md
date:                  2024-02-01T21:59:04.493166-07:00
model:                 gpt-4-0125-preview
simple_title:         "In ra thông tin debug"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/vba/printing-debug-output.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Việc in ra thông tin gỡ lỗi trong Visual Basic for Applications (VBA) bao gồm việc đặt chiến lược các câu lệnh in trong mã của bạn để hiển thị giá trị biến, luồng thực thi hoặc các thông báo gỡ lỗi tùy chỉnh. Kỹ thuật này rất cần thiết cho việc gỡ lỗi, giúp các lập trình viên hiểu được hành vi của mã của họ trong quá trình chạy và nhận diện bất kỳ hành vi không mong đợi hoặc lỗi nào.

## Cách làm:
Trong VBA, câu lệnh `Debug.Print` là công cụ chính để in thông tin gỡ lỗi ra Cửa sổ Ngay lập tức (Immediate Window) trong trình biên tập Visual Basic (VBE). Để sử dụng tính năng này một cách hiệu quả, bạn cần hiển thị Cửa sổ Ngay lập tức (Xem > Cửa sổ Ngay lập tức hoặc nhấn `Ctrl+G` trong VBE).

Dưới đây là một ví dụ đơn giản về việc sử dụng `Debug.Print` để xuất giá trị của một biến và một thông báo tùy chỉnh:

```basic
Sub PrintDebugInfo()
    Dim sampleVar As Integer
    sampleVar = 42
    Debug.Print "Giá trị của sampleVar là: "; sampleVar
End Sub
```

Khi bạn chạy hàm con này, Cửa sổ Ngay lập tức sẽ hiển thị:
```
Giá trị của sampleVar là: 42
```

Bạn cũng có thể sử dụng nó để theo dõi luồng của logic điều kiện phức tạp bằng cách chèn các câu lệnh `Debug.Print` vào các nhánh khác nhau của mã bạn:

```basic
Sub CheckValue()
    Dim valueToCheck As Integer
    valueToCheck = 9
    
    If valueToCheck > 10 Then
        Debug.Print "Giá trị lớn hơn 10."
    ElseIf valueToCheck < 10 And valueToCheck > 0 Then
        Debug.Print "Giá trị nằm giữa 1 và 9."
    Else
        Debug.Print "Giá trị là 10 hoặc nhỏ hơn 1."
    End If
End Sub
```

Khi chạy `CheckValue` sẽ xuất ra:
```
Giá trị nằm giữa 1 và 9.
```

Hãy nhớ, đầu ra từ `Debug.Print` chỉ đi đến Cửa sổ Ngay lập tức, điều này rất hữu ích trong giai đoạn phát triển nhưng không xuất hiện trong bất kỳ bộ phận nào đối diện với người dùng của ứng dụng.

## Sâu hơn
Cửa sổ Ngay lập tức và phương thức `Debug.Print` có nguồn gốc sâu xa trong lịch sử của Visual Basic for Applications, phản ánh sự phát triển của thực hành gỡ lỗi theo thời gian. Ban đầu, việc gỡ lỗi là quá trình mang tính chất văn bản và ít hình ảnh hơn, với các nhà phát triển dựa nhiều vào các câu lệnh in để hiểu những gì mã của họ đang làm. Qua nhiều năm, cùng với sự phát triển của môi trường phát triển, các công cụ gỡ lỗi cũng phát triển, giới thiệu các điểm dừng (breakpoints), theo dõi (watches), và các công cụ đánh giá chuyên sâu hơn cung cấp cái nhìn trực tiếp và tương tác hơn về hành vi mã.

Tuy nhiên, `Debug.Print` và Cửa sổ Ngay lập tức vẫn rất hữu ích, đặc biệt là cho các phiên gỡ lỗi nhanh chóng hoặc khi đối mặt với mã khó phân tích (như trình xử lý sự kiện). Tuy nhiên, quan trọng là phải nhận ra rằng việc chỉ dựa vào các câu lệnh in để gỡ lỗi trong lập trình hiện đại có thể kém hiệu quả so với việc sử dụng các trình gỡ lỗi tích hợp với khả năng kiểm tra điểm dừng, theo dõi và kiểm tra ngăn xếp.

Mặc dù các phương thức khác như khung làm việc đăng nhập hoặc các công cụ gỡ lỗi tiên tiến hơn cung cấp nhiều tính năng và linh hoạt hơn, tính đơn giản và ngay lập tức của `Debug.Print` trong VBA làm cho nó trở thành một công cụ quý giá, đặc biệt là cho các lập trình viên chuyển từ các ngôn ngữ khác đã quen với các kỹ thuật gỡ lỗi dựa trên việc in. Tuy nhiên, khi họ trở nên thoải mái hơn với VBA và trình biên tập Visual Basic, việc khám phá toàn bộ phạm vi của các công cụ gỡ lỗi có sẵn có thể dẫn đến việc giải quyết vấn đề một cách hiệu quả và hiệu quả hơn.
