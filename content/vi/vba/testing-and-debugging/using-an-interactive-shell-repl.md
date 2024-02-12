---
title:                "Sử dụng vỏ tương tác (REPL)"
aliases:
- /vi/vba/using-an-interactive-shell-repl.md
date:                  2024-02-01T22:04:24.780742-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng vỏ tương tác (REPL)"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/vba/using-an-interactive-shell-repl.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Một shell tương tác, hoặc Vòng lặp Đọc-Đánh giá-In (REPL), cho phép người dùng nhập lệnh, thực thi chúng và xem kết quả ngay lập tức. Lập trình viên sử dụng REPLs cho việc tạo mẫu nhanh, thử nghiệm đoạn mã, hoặc sửa lỗi một cách linh hoạt và lặp đi lặp lại, tăng cường năng suất và hiểu biết về mã lệnh.

## Cách thực hiện:

Visual Basic for Applications (VBA) bản thân nó không hỗ trợ tự nhiên một shell tương tác hay trải nghiệm REPL như những ngôn ngữ như Python hoặc JavaScript. Tuy nhiên, bạn có thể mô phỏng trải nghiệm này đến một chừng mực nhất định sử dụng Cửa sổ Lập tức trong IDE VBA (Môi trường Phát triển Tích hợp).

**Truy cập Cửa sổ Lập tức:**
1. Mở IDE VBA bằng cách nhấn `Alt + F11` trong ứng dụng Office của bạn.
2. Nếu Cửa sổ Lập tức không hiển thị, bạn có thể mở nó bằng cách nhấn `Ctrl + G` hoặc chọn nó từ menu Xem.

**Sử dụng Cửa sổ Lập tức như một REPL:**
- Để thực thi một dòng mã, chỉ cần nhập nó vào Cửa sổ Lập tức và nhấn Enter. Ví dụ:

```basic
Debug.Print 2 + 2
```

- Kết quả Mẫu:
```
 4
```

- Bạn cũng có thể gọi các hàm và subroutine được định nghĩa trong các mô-đun của bạn:

```basic
Public Sub SayHello()
    Debug.Print "Hello, World!"
End Sub
```

- Và sau đó trong Cửa sổ Lập tức:
```basic
Call SayHello
```

- Kết quả Mẫu:
```
 Hello, World!
```

**Chú ý:** Cửa sổ Lập tức có hạn chế. Nó tuyệt vời cho các thử nghiệm nhanh và gọi hàm trực tiếp, nhưng nó không hỗ trợ việc định nghĩa hàm hoặc subroutine trực tiếp trong đó. Các nhiệm vụ sửa lỗi và lập trình phức tạp có thể cần đến việc phát triển mô-đun đầy đủ.

## Sâu hơn

Cửa sổ Lập tức trong VBA phục vụ như một đối tác gần nhất với các shell tương tác tìm thấy trong các hệ sinh thái lập trình khác, mặc dù có những hạn chế. Lịch sử về VBA đã tập trung vào việc mở rộng khả năng của các ứng dụng Microsoft Office thông qua các kịch bản và macro hơn là phát triển phần mềm độc lập, có thể giải thích sự vắng mặt của một REPL đầy đủ.

Đối với các nhiệm vụ cần đến thử nghiệm tương tác rộng rãi hoặc phát triển logic phức tạp, các môi trường lập trình khác được trang bị hỗ trợ REPL tự nhiên, như Python với IDLE của nó, hoặc JavaScript với Node.js, có thể cung cấp những lựa chọn tốt hơn. Những môi trường này cung cấp không chỉ các shell tương tác mà còn các tiện ích lập trình, sửa lỗi và kiểm thử mạnh mẽ hơn.

Cửa sổ Lập tức cung cấp một công cụ vô giá cho việc kiểm tra biểu thức nhanh chóng, chạy hàm, và điều khiển trực tiếp các đối tượng ứng dụng Office. Như vậy, nó chiếm một vị trí thiết yếu trong quá trình phát triển VBA, cung cấp một sự thuận tiện và khả năng tiếp cận không thể so sánh được bởi những chu kỳ biên dịch-chạy-sửa lỗi truyền thống, mặc dù có những ràng buộc được hiểu về phạm vi hoạt động của nó.
