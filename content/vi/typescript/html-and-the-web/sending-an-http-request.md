---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:55.013001-07:00
description: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP l\xE0 c\xE1ch ch\u01B0\u01A1\
  ng tr\xECnh c\u1EE7a b\u1EA1n y\xEAu c\u1EA7u d\u1EEF li\u1EC7u t\u1EEB m\u1ED9\
  t m\xE1y ch\u1EE7 ho\u1EB7c g\u1EEDi d\u1EEF li\u1EC7u \u0111\u1EBFn m\xE1y ch\u1EE7\
  . L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y v\xEC \u0111\
  \xF3 l\xE0\u2026"
lastmod: '2024-02-25T18:49:34.640202-07:00'
model: gpt-4-0125-preview
summary: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP l\xE0 c\xE1ch ch\u01B0\u01A1ng tr\xEC\
  nh c\u1EE7a b\u1EA1n y\xEAu c\u1EA7u d\u1EEF li\u1EC7u t\u1EEB m\u1ED9t m\xE1y ch\u1EE7\
  \ ho\u1EB7c g\u1EEDi d\u1EEF li\u1EC7u \u0111\u1EBFn m\xE1y ch\u1EE7. L\u1EADp tr\xEC\
  nh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y v\xEC \u0111\xF3 l\xE0\u2026"
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP"
---

{{< edit_this_page >}}

## Gì và Tại sao?

Gửi một yêu cầu HTTP là cách chương trình của bạn yêu cầu dữ liệu từ một máy chủ hoặc gửi dữ liệu đến máy chủ. Lập trình viên thực hiện điều này vì đó là nền tảng của việc tương tác với các dịch vụ web, API, và nguồn tài nguyên từ xa.

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
