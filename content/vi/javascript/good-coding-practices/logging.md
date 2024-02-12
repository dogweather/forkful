---
title:                "Ghi log"
aliases: - /vi/javascript/logging.md
date:                  2024-01-28T22:03:34.854352-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi log"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/javascript/logging.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại Sao?
Ghi nhật ký, nói một cách ngắn gọn, giống như việc giữ một cuốn nhật ký cho ứng dụng của bạn—nó ghi lại các sự kiện, lỗi, và các hành động đáng chú ý khác xảy ra trong khi phần mềm đang chạy. Lập trình viên thực hiện việc này không chỉ để hiểu được điều gì đang xảy ra bên dưới góc độ kỹ thuật một cách thực tiễn, mà còn để có một bản ghi lịch sử quan trọng cho việc gỡ rối, kiểm toán, và tối ưu hóa hiệu suất.

## Làm Thế Nào:
Ngay từ cơ bản, JavaScript cung cấp một cách đơn giản để ghi những thông điệp vào bảng điều khiển:

```javascript
console.log('Thông điệp này sẽ được ghi vào bảng điều khiển');

// Kết quả:
// Thông điệp này sẽ được ghi vào bảng điều khiển
```

Nhưng ứng dụng thực tế đòi hỏi nhiều hơn là chỉ in những thông điệp vào bảng điều khiển. Các thư viện như Winston hay Pino có thể được giới thiệu để quản lý nhật ký một cách hiệu quả:

```javascript
// Sử dụng Winston cho ghi nhật ký nâng cao
const winston = require('winston');

const logger = winston.createLogger({
  level: 'info',
  format: winston.format.json(),
  transports: [
    new winston.transports.File({ filename: 'combined.log' })
  ],
});

logger.info('Xin chào, đây là một sự kiện ghi nhật ký với Winston');
// Nhật ký này được viết vào 'combined.log' ở dạng JSON
```

Mẫu kết quả `combined.log`:

```json
{"message":"Xin chào, đây là một sự kiện ghi nhật ký với Winston","level":"info"}
```

## Sâu Hơn
Ghi nhật ký đã trở nên thiết yếu từ những ngày đầu của ngành công nghiệp máy tính; các nhà vận hành hệ thống sẽ kiểm tra nhật ký để hiểu hiệu suất hệ thống và chẩn đoán vấn đề. Nhanh chóng chuyển mình tới phát triển hiện đại, và chúng ta đã chuyển từ các tệp nhật ký đơn giản sang các hệ thống quản lý nhật ký có cấu trúc và có thể tìm kiếm.

Các lựa chọn thay thế cho việc ghi nhật ký vào bảng điều khiển hoặc tệp trong JavaScript bao gồm việc sử dụng các dịch vụ ghi nhật ký dựa trên đám mây như Loggly, Datadog, hoặc ELK Stack (Elasticsearch, Logstash, Kibana) có thể tổng hợp nhật ký từ nhiều nguồn, cung cấp công cụ trực quan và phân tích nâng cao.

Khi triển khai ghi nhật ký, hãy xem xét những điều sau:
- **Mức Độ Chi Tiết**: Bao gồm gỡ lỗi, thông tin, cảnh báo, lỗi, và nguy kịch.
- **Hiệu Suất**: Ghi nhật ký quá mức có thể ảnh hưởng đến hiệu suất ứng dụng.
- **Bảo Mật**: Cẩn thận khi ghi nhật ký thông tin nhạy cảm.
- **Định Dạng**: Nhật ký có cấu trúc (như JSON) giúp dễ dàng tìm kiếm và phân tích nhật ký hơn.
- **Chính Sách Lưu Trữ**: Nhật ký cũ cần được lưu trữ hoặc xóa bỏ để tiết kiệm không gian.

Một chiến lược ghi nhật ký thực tế định rõ nên ghi gì, ghi ở đâu, và giữ nó trong bao lâu, cân nhắc giữa thông tin thông tin chi tiết và các xem xét về hiệu suất và quyền riêng tư.

## Xem Thêm
Xem những nguồn thông tin này để hiểu sâu hơn:
- [Kho GitHub Winston](https://github.com/winstonjs/winston): để biết cách sử dụng chi tiết và vận chuyển tùy chỉnh.
- [Pino - Bộ nhật ký Node.js rất nhẹ](https://github.com/pinojs/pino): một giải pháp ghi nhật ký nhẹ.
- [MDN Web Docs: Bảng Điều Khiển](https://developer.mozilla.org/en-US/docs/Web/API/Console): để biết thông tin cơ bản về ghi nhật ký dựa trên trình duyệt.
- [Elastic ELK Stack](https://www.elastic.co/what-is/elk-stack): một bộ ba mạnh mẽ để quản lý nhật ký.
- [Ghi Nhật Ký Ứng Dụng 12 Yếu Tố](https://12factor.net/logs): các phương pháp hay nhất trong ghi nhật ký ứng dụng.
