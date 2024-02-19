---
aliases:
- /vi/google-apps-script/starting-a-new-project/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:15.184757-07:00
description: "Kh\u1EDFi \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi tr\xEAn Google\
  \ Apps Script (GAS) bao g\u1ED3m vi\u1EC7c kh\u1EDFi t\u1EA1o m\u1ED9t file script\
  \ b\xEAn trong h\u1EC7 sinh th\xE1i c\u1EE7a Google (Google Drive, Docs, Sheets,\u2026"
lastmod: 2024-02-18 23:08:50.220497
model: gpt-4-0125-preview
summary: "Kh\u1EDFi \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi tr\xEAn Google Apps\
  \ Script (GAS) bao g\u1ED3m vi\u1EC7c kh\u1EDFi t\u1EA1o m\u1ED9t file script b\xEA\
  n trong h\u1EC7 sinh th\xE1i c\u1EE7a Google (Google Drive, Docs, Sheets,\u2026"
title: "Kh\u1EDFi \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi"
---

{{< edit_this_page >}}

## Bản Chất & Lý do?

Khởi đầu một dự án mới trên Google Apps Script (GAS) bao gồm việc khởi tạo một file script bên trong hệ sinh thái của Google (Google Drive, Docs, Sheets, v.v.) để tự động hóa các nhiệm vụ hoặc mở rộng chức năng của các ứng dụng Google. Các lập trình viên thường bắt đầu hành trình này để tối ưu hóa quy trình làm việc, điều khiển các dịch vụ của Google một cách lập trình hoặc tạo ra các add-ons tùy chỉnh, tiết kiệm thời gian và khai thác sức mạnh của cơ sở hạ tầng của Google.

## Cách thức:

Để bắt đầu một dự án mới trên Google Apps Script, bạn có một vài điểm nhập, nhưng hãy tập trung vào phương thức trực tiếp nhất: tạo một script từ Google Drive.

1. **Tạo một Dự án trong Google Drive**
   - Điều hướng đến Google Drive (drive.google.com).
   - Nhấn "+ Mới" > "Thêm" > "Google Apps Script".
   - Một dự án script mới mở ra trong trình soạn thảo. Mặc định, nó chứa một file `Code.gs` với một hàm mẫu `myFunction`.

2. **Thiết lập Dự án của Bạn**
   - Đổi tên dự án để dễ nhận biết. Nhấp vào "Dự án chưa có tiêu đề" ở góc trái phía trên, và đặt cho nó một tên có ý nghĩa.
   - Viết một hàm đơn giản trong file `Code.gs` để cảm nhận:

```javascript
function helloWorld() {
  Logger.log('Chào, thế giới!');
}
```

   - Chạy `helloWorld` bằng cách chọn hàm trong dropdown bên cạnh nút phát (▶) và nhấn vào đó. Điều này sẽ thực thi hàm.

3. **Xem Nhật ký**
   - Để xem đầu ra của `Logger.log`, đi đến "Xem" > "Nhật ký", hoặc nhấn `Ctrl + Enter`. Bạn sẽ thấy "Chào, thế giới!" trong các nhật ký.

Xin chúc mừng, bạn vừa khởi đầu thành công một dự án mới trên Google Apps Script và đã chạy một hàm đơn giản!

## Sâu hơn

Sự khởi đầu của Google Apps Script vào khoảng 2009 đã cung cấp một nền tảng mạnh mẽ nhưng dễ tiếp cận cho cả nhà phát triển lẫn người không chuyên về phát triển để tự động hóa, mở rộng, và xây dựng trên bộ sưu tập đồ sộ các dịch vụ của Google. Không giống như môi trường lập trình truyền thống, GAS mang lại một sự pha trộn độc đáo giữa sự đơn giản và tích hợp, trực tiếp trong hệ sinh thái của Google, mà không cần máy chủ bên ngoài hay cài đặt. Mô hình thực thi không máy chủ này giúp đơn giản hóa rất nhiều việc triển khai và quản lý dự án.

Về mặt lịch sử, GAS cũng từng bị hạn chế bởi môi trường thực thi và phiên bản ngôn ngữ của nó, thường đi sau so với các tiêu chuẩn JavaScript hiện tại. Tuy nhiên, các cập nhật gần đây đã đưa cú pháp JavaScript hiện đại (ECMAScript 2015+) vào GAS, làm cho nó trở nên hấp dẫn hơn đối với các nhà phát triển quen với các phương pháp phát triển hiện đại.

Mặc dù GAS có vị thế độc đáo trong việc tương tác với các Dịch vụ của Google, vẫn có những phương pháp thay thế cho các nhu cầu chuyên sâu hoặc cụ thể hơn. Ví dụ, Google Cloud Functions và Google Cloud Platform (GCP) cung cấp các giải pháp mạnh mẽ và có khả năng mở rộng hơn cho việc xử lý các quy trình làm việc phức tạp, xử lý tập dữ liệu lớn và tích hợp với các API bên ngoài. Những nền tảng này cho phép lập trình bằng nhiều ngôn ngữ (ví dụ, Python, Go, Node.js) và cung cấp nhiều tài nguyên tính toán hơn.

Dẫu vậy, đối với các nhiệm vụ gắn liền với Google Apps, tự động hóa, và phát triển nhanh chóng trong hệ sinh thái này, Google Apps Script vẫn là một công cụ không có đối thủ về độ dễ sử dụng và độ sâu tích hợp. Sự tiện lợi trực tiếp từ Google Drive và kết nối liền mạch với các dịch vụ của Google khiến nó trở thành lựa chọn thực tế cho một loạt dự án, đặc biệt là cho những ai muốn mở rộng chức năng của Sheets, Docs, Forms và các ứng dụng Google khác.
