---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:25.769178-07:00
description: "T\u1EA1o m\u1ED9t file t\u1EA1m th\u1EDDi c\xF3 ngh\u0129a l\xE0 t\u1EA1\
  o m\u1ED9t file \u0111\u01B0\u1EE3c thi\u1EBFt k\u1EBF cho vi\u1EC7c s\u1EED d\u1EE5\
  ng ng\u1EAFn h\u1EA1n. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0\
  y v\xEC m\u1ED9t s\u1ED1 l\xFD do nh\u01B0 b\u1EA3o v\u1EC7 d\u1EEF li\u1EC7u\u2026"
lastmod: '2024-02-25T18:49:34.911393-07:00'
model: gpt-4-0125-preview
summary: "T\u1EA1o m\u1ED9t file t\u1EA1m th\u1EDDi c\xF3 ngh\u0129a l\xE0 t\u1EA1\
  o m\u1ED9t file \u0111\u01B0\u1EE3c thi\u1EBFt k\u1EBF cho vi\u1EC7c s\u1EED d\u1EE5\
  ng ng\u1EAFn h\u1EA1n. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0\
  y v\xEC m\u1ED9t s\u1ED1 l\xFD do nh\u01B0 b\u1EA3o v\u1EC7 d\u1EEF li\u1EC7u\u2026"
title: "T\u1EA1o m\u1ED9t t\u1EADp tin t\u1EA1m th\u1EDDi"
---

{{< edit_this_page >}}

# Tạo file tạm thời với Elm

## Cái gì & Tại sao?

Tạo một file tạm thời có nghĩa là tạo một file được thiết kế cho việc sử dụng ngắn hạn. Các lập trình viên làm điều này vì một số lý do như bảo vệ dữ liệu nhạy cảm hoặc quản lý kết quả tạm thời trong quá trình xử lý.

## Cách thực hiện:

Elm chạy trong trình duyệt, vì vậy nó không có quyền truy cập hệ thống file trực tiếp. Do đó, bạn không thể tạo file tạm thời theo cách truyền thống. Nhưng, nếu bạn cần một tính năng tương tự, chúng tôi sử dụng cổng Elm (Elm ports) để tương tác với JavaScript, có thể xử lý việc tạo file tạm thời.

```elm
port module Main exposing (..)

-- Định nghĩa một cổng để tạo file tạm thời trong JavaScript
port createTempFile : String -> Cmd msg

-- Gửi dữ liệu sang JavaScript để tạo file tạm thời
saveDataTemporarily : String -> Cmd msg
saveDataTemporarily data =
    createTempFile data
```

Đối với phần JavaScript, sử dụng File API:

```javascript
app.ports.createTempFile.subscribe(function(data) {
    var blob = new Blob([data], {type: 'text/plain'});
    var url = URL.createObjectURL(blob);

    // Tại đây bạn có thể sử dụng URL để tải xuống blob hoặc chuyển nó sang các phần khác của ứng dụng
    console.log(url);  // Nó ghi log URL của file tạm thời
});
```

Mẫu đầu ra trong bảng điều khiển JavaScript:

```plaintext
blob:null/2135a9b7-1aad-4e7a-8bce-19c4f3f6d7ff
```

## Tìm hiểu sâu hơn

Elm được thiết kế để an toàn và đáng tin cậy, vì vậy quyền truy cập hệ thống file trực tiếp không có trong danh sách. Thay vào đó, Elm sử dụng cổng để giao tiếp với JavaScript, cho phép các thao tác như tạo file tạm thời. Theo lịch sử, chúng tôi xử lý các nhiệm vụ dựa trên file trong trình duyệt thông qua các API của JavaScript, sử dụng Elm cho logic cấp cao, an toàn về kiểu.

Các phương án khác như WebAssembly có thể cho phép tương tác hệ thống file trực tiếp nhiều hơn trong tương lai, nhưng hiện nay, tương tác với JavaScript là thực hành tiêu chuẩn.

Về mặt triển khai, việc tạo file tạm thời trong bối cảnh trình duyệt không có nghĩa là một file thực sự trên hệ thống file, mà là một biểu diễn trong bộ nhớ (blob) mà bạn có thể làm việc và lưu trữ nếu cần.

## Xem thêm

- [Cổng Elm](https://guide.elm-lang.org/interop/ports.html)
- [MDN - Web APIs - File](https://developer.mozilla.org/en-US/docs/Web/API/File)
- [MDN - Web APIs - Blob](https://developer.mozilla.org/en-US/docs/Web/API/Blob)
