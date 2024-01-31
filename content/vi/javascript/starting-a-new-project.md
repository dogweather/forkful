---
title:                "Bắt đầu một dự án mới"
date:                  2024-01-28T22:08:56.032454-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bắt đầu một dự án mới"

category:             "Javascript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/javascript/starting-a-new-project.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Bắt đầu một dự án mới có nghĩa là thiết lập một cơ sở mã nguồn mới cho những ý tưởng tuyệt vời của bạn. Các lập trình viên làm điều này để biến các khái niệm thành các ứng dụng hoặc dịch vụ hoạt động thực sự.

## Cách thực hiện:

Trước khi viết mã, hãy quyết định về công cụ và cấu trúc. Chúng ta sử dụng Node.js và npm (Node Package Manager) cho ví dụ này.

1. Cài đặt Node.js từ [trang web chính thức](https://nodejs.org/).
2. Mở terminal và chạy:

```javascript
npm init
```

Trả lời các câu hỏi thiết lập. Bùm—`package.json` được tạo, mô tả dự án của bạn. Tiếp theo, hãy thêm Express, một framework web phổ biến:

```javascript
npm install express --save
```

Bây giờ, viết một máy chủ web đơn giản trong `index.js`:

```javascript
const express = require('express');
const app = express();

app.get('/', (req, res) => {
  res.send('Hello World!');
});

app.listen(3000, () => {
  console.log('Server is up on port 3000');
});
```

Chạy máy chủ của bạn:

```javascript
node index.js
```

Đầu ra mẫu:

```
Server is up on port 3000
```

Điều hướng đến `http://localhost:3000` trên trình duyệt web của bạn. Bạn nên thấy "Hello World!".

## Sâu hơn

Trước kia, việc thiết lập dự án là một cơn đau đầu, với rất nhiều cấu hình thủ công. Ngày nay, các công cụ như npm đã làm phần nặng nhọc. Đối với các dự án front-end, hãy cân nhắc `create-react-app` hoặc `vue-cli`. Đối với Node.js, Express là một lựa chọn vững chắc, cân bằng giữa công suất và đơn giản. Nó nhẹ nhưng có các tính năng mạnh mẽ cho hầu hết nhu cầu máy chủ web.

Nhớ rằng, cách bạn tổ chức dự án của mình rất quan trọng. Các ứng dụng Node.js truyền thống có một điểm nhập (như `index.js`), một tệp `package.json` để quản lý các phụ thuộc, và cấu trúc thư mục phân chia các mối quan tâm (mô-đun, tiện ích, tuyến đường, v.v.).

Các lựa chọn thay thế cho npm để quản lý gói bao gồm Yarn, mang lại các cải tiến về tốc độ và nhất quán. Đối với việc khung scaffolding dự án, Yeoman giúp đỡ bằng cách cung cấp các bộ sinh cho nhiều loại dự án và công nghệ.

## Xem thêm

- Tài liệu Node.js [docs](https://nodejs.org/en/docs/)
- Trang chính thức Express [official site](https://expressjs.com/)
- Kho GitHub `create-react-app` [GitHub repo](https://github.com/facebook/create-react-app)
- Tài liệu Vue CLI [docs](https://cli.vuejs.org/)
- Trang chính thức Yarn [official site](https://classic.yarnpkg.com/lang/en/)
- Trang chính thức Yeoman [official site](http://yeoman.io/)
