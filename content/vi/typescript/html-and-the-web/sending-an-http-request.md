---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:55.013001-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong TypeScript, b\u1EA1n th\u01B0\u1EDDng\
  \ s\u1EED d\u1EE5ng Fetch API \u0111\u1EC3 g\u1EEDi c\xE1c y\xEAu c\u1EA7u HTTP.\
  \ D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t v\xED d\u1EE5 nhanh, s\u1EED d\u1EE5\
  ng `async/await` \u0111\u1EC3 \u0111\u01A1n gi\u1EA3n h\xF3a."
lastmod: '2024-03-13T22:44:36.314754-06:00'
model: gpt-4-0125-preview
summary: "Trong TypeScript, b\u1EA1n th\u01B0\u1EDDng s\u1EED d\u1EE5ng Fetch API\
  \ \u0111\u1EC3 g\u1EEDi c\xE1c y\xEAu c\u1EA7u HTTP."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP"
weight: 44
---

## Làm thế nào:
Trong TypeScript, bạn thường sử dụng Fetch API để gửi các yêu cầu HTTP. Dưới đây là một ví dụ nhanh, sử dụng `async/await` để đơn giản hóa:

```typescript
async function fetchData(url: string): Promise<void> {
  try {
    const response = await fetch(url);
    if (!response.ok) {
      throw new Error(`Lỗi HTTP! trạng thái: ${response.status}`);
    }
    const data = await response.json();
    console.log(data);
  } catch (error) {
    console.error('Lỗi fetch:', error);
  }
}

fetchData('https://jsonplaceholder.typicode.com/todos/1');
```

Kết quả mẫu cho một yêu cầu thành công:

```json
{
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

## Tìm hiểu sâu
Yêu cầu HTTP đã trở thành yếu tố quan trọng kể từ bình minh của web; chúng là cách trình duyệt và máy chủ giao tiếp. Trước khi `fetch` xuất hiện, XMLHttpRequest (XHR) đã làm công việc đó nhưng cảm giác như làm thủ tục giấy tờ. `fetch`, một lựa chọn hiện đại, dựa trên promise, sạch sẽ hơn, và là một phần của đối tượng window trong hầu hết các trình duyệt hiện đại.

Các lựa chọn thay thế cho `fetch` trong TypeScript bao gồm các thư viện như Axios, cung cấp nhiều tính năng hơn và đôi khi dễ xử lý hơn. Axios tự động chuyển đổi dữ liệu JSON, xử lý huỷ yêu cầu và cung cấp cách xử lý lỗi tốt hơn.

Đằng sau hậu trường, TypeScript biên dịch xuống JavaScript. Khi bạn gửi một yêu cầu HTTP sử dụng `fetch`, bạn cơ bản đang sử dụng API Fetch native của trình duyệt. Kiểm tra kiểu của TypeScript tăng cường sự ổn định của mã của bạn bằng cách bắt lỗi kiểu khi biên dịch.

## Tham khảo thêm
- MDN Web Docs về Fetch: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API
- Kho GitHub của Axios: https://github.com/axios/axios
- So sánh các thư viện yêu cầu HTTP: https://www.npmtrends.com/axios-vs-fetch
