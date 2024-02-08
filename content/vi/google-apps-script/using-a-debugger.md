---
title:                "Sử dụng trình gỡ lỗi"
aliases:
- vi/google-apps-script/using-a-debugger.md
date:                  2024-02-01T22:03:47.487104-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng trình gỡ lỗi"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/google-apps-script/using-a-debugger.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Quá trình gỡ lỗi trong Google Apps Script (GAS) bao gồm việc xác định và loại bỏ lỗi từ các kịch bản được thiết kế để tự động hóa Google Apps hoặc xây dựng ứng dụng web. Lập trình viên gỡ lỗi để đảm bảo mã của họ thực thi như mong đợi, nâng cao độ tin cậy và hiệu suất trong ứng dụng.

## Làm thế nào:

Google Apps Script cung cấp một trình gỡ lỗi tích hợp sẵn trong Trình Biên Tập Apps Script để giúp khắc phục sự cố kịch bản. Dưới đây là cách khởi chạy và sử dụng trình gỡ lỗi:

1. **Mở kịch bản của bạn trong Trình Biên Tập Apps Script.**
2. **Chọn một hàm để gỡ lỗi.** Từ menu thả xuống ở phía trên, chọn hàm bạn muốn gỡ lỗi.
3. **Đặt điểm dừng.** Nhấp vào gutter (khu vực màu xám bên trái của các số dòng) nơi bạn muốn tạm dừng thực thi; một dấu chấm đỏ xuất hiện, biểu thị một điểm dừng.
4. **Bắt đầu gỡ lỗi.** Nhấp vào biểu tượng bọ hoặc chọn `Gỡ lỗi` > `Bắt đầu gỡ lỗi`. Thực thi sẽ bắt đầu và tạm dừng tại điểm dừng đầu tiên.

Xem xét đoạn script đơn giản này:

```javascript
function calculateSum() {
  var a = 5;
  var b = 10;
  var sum = a + b;
  Logger.log(sum); // Dự định ghi log 15
}
```

Nếu không chắc chắn vì sao `Logger.log(sum)` không hiển thị kết quả mong đợi, bạn có thể đặt một điểm dừng tại dòng `var sum = a + b;` và bước qua từng dòng script để kiểm tra giá trị của biến.

**Kết quả mẫu trong Logger:**

```plain
15
```

Trong khi gỡ lỗi, Trình Biên Tập Apps Script cho phép bạn:

- **Bước qua mã** sử dụng các nút bước qua, bước vào và bước ra.
- **Theo dõi biểu thức và biến** để xem giá trị thay đổi trực tiếp.
- **Kiểm tra ngăn xếp cuộc gọi** để truy vết các cuộc gọi hàm.

## Sâu hơn

Gỡ lỗi trong Google Apps Script, như trong bất kỳ môi trường lập trình nào khác, là thiết yếu để tạo ra các ứng dụng không lỗi. Được giới thiệu sớm trong quá trình phát triển của GAS, trình gỡ lỗi tích hợp sẵn cung cấp các khả năng cơ bản để kiểm tra và sửa mã từng bước. Mặc dù nó cung cấp các tính năng gỡ lỗi cơ bản tương tự như những môi trường chín muồi hơn như Visual Studio Code hay IntelliJ, nó có thể không đủ mạnh cho các tình huống gỡ lỗi phức tạp. Ví dụ, khả năng kiểm tra các cuộc gọi bất đồng bộ hoặc quản lý việc thực thi kịch bản nặng có thể bị hạn chế.

Đối với nhu cầu gỡ lỗi phức tạp, lập trình viên có thể tìm đến các phương pháp thay thế như lưu trữ mở rộng (sử dụng `Logger.log()`) hoặc thậm chí triển khai dưới dạng một ứng dụng web để kiểm tra hành vi trong một tình huống thực tế. Tuy nhiên, sự đơn giản và tích hợp của trình gỡ lỗi GAS trong Trình Biên Tập Apps Script làm cho nó trở thành bước đầu tiên quý báu để khắc phục sự cố và hiểu biết về hành vi kịch bản. Đáng chú ý, với sự cập nhật liên tục và cải thiện từ Google đối với Apps Script, trải nghiệm gỡ lỗi đang dần cải thiện, cung cấp nhiều công cụ và lựa chọn tinh vi hơn theo thời gian. Sự phát triển này phản ánh cam kết của Google trong việc biến Apps Script thành một nền tảng mạnh mẽ và dễ tiếp cận hơn cho các lập trình viên từ nhiều lĩnh vực khác nhau.
