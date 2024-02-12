---
title:                "Xử lý lỗi"
date:                  2024-02-01T21:55:35.183254-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xử lý lỗi"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/google-apps-script/handling-errors.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Xử lý lỗi trong Google Apps Script là việc dự đoán, bắt và phản hồi trước các ngoại lệ hoặc lỗi xảy ra trong quá trình thực thi script. Các lập trình viên thực hiện xử lý lỗi để bảo vệ script khỏi các sự cố không mong muốn, đảm bảo ứng dụng hoạt động mượt mà, thân thiện với người dùng có thể quản lý hoặc ghi lại lỗi một cách nhẹ nhàng mà không bị sập đột ngột.

## Làm thế nào:

Google Apps Script, dựa trên JavaScript, cho phép chúng ta sử dụng câu lệnh `try-catch` truyền thống để xử lý lỗi, kèm theo `finally` nếu cần dọn dẹp bất kể thành công hay lỗi.

```javascript
function myFunction() {
  try {
    // Mã có thể phát sinh lỗi
    var sheet = SpreadsheetApp.getActiveSheet();
    var data = sheet.getRange("A1").getValue();
    if (data === "") {
      throw new Error("Ô A1 trống.");
    }
    Logger.log(data);
  } catch (e) {
    // Mã xử lý lỗi
    Logger.log("Lỗi: " + e.message);
  } finally {
    // Mã dọn dẹp, thực thi cho dù có lỗi xảy ra hay không
    Logger.log("Chức năng đã hoàn thành.");
  }
}
```

Kết quả mẫu không có lỗi:
```
[Giá trị ô]
Chức năng đã hoàn thành.
```

Kết quả mẫu với lỗi (giả sử A1 trống):
```
Lỗi: Ô A1 trống.
Chức năng đã hoàn thành.
```

Google Apps Script cũng hỗ trợ việc ném các lỗi tùy chỉnh bằng đối tượng `Error` và bắt các loại lỗi cụ thể nếu cần. Tuy nhiên, sự thiếu vắng của việc phân loại lỗi nâng cao khiến việc dựa vào thông điệp lỗi để xác định cụ thể trở nên cần thiết.

## Sâu hơn

Trong lịch sử, xử lý lỗi trong các ngôn ngữ kịch bản như JavaScript (và theo đó là Google Apps Script) đã kém phức tạp hơn so với một số ngôn ngữ biên dịch, có các tính năng như hệ thống phân loại ngoại lệ chi tiết và công cụ gỡ lỗi toàn diện. Mô hình của Google Apps Script tương đối đơn giản, tận dụng mô hình `try-catch-finally` của JavaScript. Sự đơn giản này phù hợp với thiết kế ngôn ngữ để phát triển và triển khai nhanh chóng các ứng dụng quy mô nhỏ đến trung bình trong hệ sinh thái của Google, nhưng nó đôi khi có thể hạn chế các lập trình viên xử lý các tình huống lỗi phức tạp.

Trong các ứng dụng phức tạp hơn, lập trình viên thường bổ sung xử lý lỗi địa phương của Google Apps Script bằng cách tùy chỉnh ghi và báo cáo lỗi. Điều này có thể bao gồm viết lỗi vào một Google Sheet cho mục đích kiểm toán hoặc sử dụng dịch vụ ghi nhận bên thứ ba thông qua Dịch vụ URL Fetch của Google Apps Script để gửi chi tiết lỗi ra khỏi môi trường script.

Mặc dù Google Apps Script có thể tụt hậu so với các ngôn ngữ như Java hay C# về độ phức tạp và khả năng xử lý lỗi tích hợp, sự kết hợp với các dịch vụ Google và sự đơn giản của cách tiếp cận `try-catch-finally` làm cho nó trở thành một công cụ mạnh mẽ cho các lập trình viên để tự động hóa nhanh chóng các nhiệm vụ và tạo các tích hợp trong hệ sinh thái Google. Các nhà phát triển từ các nền tảng khác có thể thấy thách thức không nằm ở việc chinh phục các mẫu xử lý lỗi phức tạp mà ở việc sáng tạo tận dụng những gì có sẵn để đảm bảo các script của họ mạnh mẽ và thân thiện với người dùng.
