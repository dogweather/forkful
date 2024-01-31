---
title:                "Ghi log"
date:                  2024-01-28T22:03:18.621551-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi log"

category:             "TypeScript"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/typescript/logging.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Logging là quá trình ghi lại các sự kiện, lỗi và các thông tin đáng chú ý khác trong quá trình thực thi của một chương trình vào một phương tiện ngoại vi, thường là các tệp hoặc cơ sở dữ liệu. Lập trình viên sử dụng log để theo dõi hành vi của phần mềm, gỡ lỗi vấn đề và theo dõi các hoạt động hệ thống cho phân tích an ninh và hiệu năng.

## Cách thức:

Trong TypeScript, bạn có thể dễ dàng triển khai logging cơ bản bằng cách sử dụng các phương thức console hoặc tích hợp logging nâng cao hơn với các thư viện như `winston` hoặc `pino`. Dưới đây là một ví dụ cơ bản sử dụng `console.log` và một ví dụ nâng cao hơn với `winston`.

```TypeScript
// Logging cơ bản với console
console.log('Thông tin: Khởi động ứng dụng...');
console.error('Lỗi: Không thể truy xuất dữ liệu.');

// Mẫu Đầu ra
// Thông tin: Khởi động ứng dụng...
// Lỗi: Không thể truy xuất dữ liệu.
```

Để log một cách mạnh mẽ hơn, hãy thiết lập `winston`:

```TypeScript
import { createLogger, format, transports } from 'winston';

const logger = createLogger({
  level: 'info',
  format: format.combine(
    format.timestamp({ format: 'YYYY-MM-DD HH:mm:ss' }),
    format.printf(info => `${info.timestamp} ${info.level}: ${info.message}`)
  ),
  transports: [
    new transports.Console(),
    new transports.File({ filename: 'combined.log' })
  ]
});

logger.info('Máy chủ đã khởi động!');
logger.warn('Cảnh báo không gian đĩa thấp.');
logger.error('Không thể kết nối với cơ sở dữ liệu.');

// Mẫu Đầu ra trong combined.log
// 2023-01-20 14:42:07 info: Máy chủ đã khởi động!
// 2023-01-20 14:42:09 warn: Cảnh báo không gian đĩa thấp.
// 2023-01-20 14:42:12 error: Không thể kết nối với cơ sở dữ liệu.
```

## Khám Phá Sâu:

Khái niệm về logging trong ngữ cảnh máy tính có từ những ngày đầu của lập trình, nơi mà thuật ngữ này được xuất phát từ "sổ nhật ký" (logbook), một hệ thống ghi chép hàng hải. Lịch sử, các sự kiện chương trình thường được logged vào các bản in vật lý hoặc đầu ra terminal, đặc biệt là trong kỷ nguyên mainframe.

Nhanh chóng chuyển tới ngày nay, và bạn có một lượng lớn công cụ và thư viện tùy ý sử dụng phục vụ cho các nhu cầu logging đa dạng, từ các tệp văn bản đơn giản đến các hệ thống quản lý log phức tạp. Các lựa chọn thay thế cho `winston` bao gồm `pino`, được ca ngợi về hiệu suất cao, và `Bunyan`, dựa trên JSON. Khi làm việc với Node.js, các thư viện logging thường cung cấp cơ chế luồng để định hướng log đến các điểm đến khác nhau, hỗ trợ cho việc xoay vòng log và các bộ định dạng tùy chỉnh.

Về mặt triển khai, thông điệp log thường chứa một dấu thời gian, một mức độ nghiêm trọng (như info, warn, error), và thông điệp thực tế. Thực hành logging tốt đề xuất phân loại đúng mức độ log, tránh dữ liệu nhạy cảm trong log, và xem xét tới hậu quả về hiệu suất trong các ứng dụng có lưu lượng cao.

## Xem Thêm:

- [Winston - Một logger cho hầu như mọi thứ](https://www.npmjs.com/package/winston)
- [Pino - Logger Node.js với hiệu suất cực kỳ thấp](https://www.npmjs.com/package/pino)
- [Phương pháp Logging tốt nhất với Node.js](https://thisdavej.com/using-winston-a-versatile-logging-library-for-node-js/)
- [Ứng dụng 12 yếu tố - Logs](https://12factor.net/logs)
