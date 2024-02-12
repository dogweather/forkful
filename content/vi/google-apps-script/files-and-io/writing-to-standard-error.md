---
title:                "Ghi vào lỗi chuẩn"
date:                  2024-02-01T22:09:14.371973-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi vào lỗi chuẩn"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/google-apps-script/writing-to-standard-error.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?

Viết lỗi chuẩn (stderr) trong ngôn ngữ lập trình là về hướng các thông báo lỗi và chẩn đoán sang một luồng riêng, tách biệt khỏi đầu ra chuẩn (stdout). Lập trình viên làm điều này để tách biệt đầu ra chương trình bình thường khỏi thông báo lỗi, làm cho việc gỡ rối và phân tích log trở nên đơn giản hơn.

## Làm thế nào:

Google Apps Script, với tư cách là ngôn ngữ kịch bản cho phát triển ứng dụng nhẹ trên nền tảng Google Apps, không cung cấp một hàm tích hợp trực tiếp như `console.error()` để viết vào stderr, như bạn có thể tìm thấy trong Node.js hay Python. Tuy nhiên, bạn có thể mô phỏng hành vi này bằng cách sử dụng dịch vụ log của Google Apps Script hoặc xử lý lỗi tùy chỉnh để quản lý và phân loại đầu ra lỗi.

### Ví dụ: Sử dụng `Logger` cho Thông báo Lỗi

```javascript
function logError() {
  try {
    // Giả lập một lỗi
    const result = 1 / 0;
    if(!isFinite(result)) throw new Error("Cố gắng chia cho zero");
  } catch (e) {
    // Ghi thông báo lỗi vào Logs
    Logger.log('Lỗi: ' + e.message);
  }
}
```

Khi bạn chạy `logError()`, điều này sẽ ghi thông báo lỗi vào log của Google Apps Script, mà bạn có thể xem bằng `Xem > Logs`. Điều này không hoàn toàn là stderr, nhưng nó phục vụ một mục đích tương tự là tách biệt log lỗi khỏi đầu ra chuẩn.

### Ghi Log Chẩn Đoán Nâng Cao

Để gỡ rối và ghi log lỗi một cách nâng cao hơn, bạn có thể sử dụng Stackdriver Logging, hiện được biết đến với tên là Bộ Công cụ Hoạt động của Google Cloud.

```javascript
function advancedErrorLogging() {
  try {
    // Cố ý gây ra lỗi
    const obj = null;
    const result = obj.someProperty;
  } catch (e) {
    console.error('Lỗi gặp phải: ', e.toString());
  }
}
```

Điều này sẽ hướng thông báo lỗi đến Stackdriver Logging, nơi nó được quản lý như một log cấp độ lỗi. Lưu ý rằng việc tích hợp Stackdriver/Bộ Công cụ Hoạt động của Google Cloud cung cấp một giải pháp log có khả năng tìm kiếm và mức độ chi tiết cao hơn so với `Logger`.

## Đào Sâu

Sự thiếu một luồng `stderr` chuyên biệt trong Google Apps Script phản ánh bản chất và nguồn gốc của nó như một ngôn ngữ kịch bản dựa trên đám mây, nơi các đầu ra dựa trên bảng điều khiển hoặc thiết bị đầu cuối truyền thống (như stdout và stderr) ít liên quan hơn. Lịch sử, Google Apps Script được thiết kế cho việc tăng cường chức năng của Google Apps bằng cách sử dụng các kịch bản đơn giản, tập trung vào sự dễ sử dụng hơn là các tính năng toàn diện có sẵn trong môi trường lập trình phức tạp hơn.

Tuy nhiên, sự phát triển của Google Apps Script hướng tới phát triển ứng dụng phức tạp hơn đã thúc đẩy các nhà phát triển áp dụng các phương pháp sáng tạo cho việc xử lý lỗi và ghi log, sử dụng các dịch vụ có sẵn như Logger và tích hợp với Bộ Công cụ Hoạt động của Google Cloud. Những phương pháp này, mặc dù không phải là các triển khai stderr trực tiếp, cung cấp các lựa chọn thay thế mạnh mẽ cho việc quản lý lỗi và ghi log chẩn đoán trong một môi trường tập trung vào đám mây.

Quan trọng, trong khi những phương pháp này phục vụ mục đích trong hệ sinh thái của Google Apps Script, chúng làm nổi bật các hạn chế của nền tảng so với các môi trường lập trình truyền thống. Đối với các nhà phát triển yêu cầu các chiến lược xử lý lỗi chi tiết và phân cấp, việc tích hợp với các dịch vụ ghi log bên ngoài hoặc áp dụng Google Cloud Functions, cung cấp xử lý stderr và stdout theo cách truyền thống có thể được ưa chuộng hơn.
