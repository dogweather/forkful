---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:36.039508-07:00
description: "L\xE0m th\u1EBF n\xE0o: Ch\xFAng ta h\xE3y t\xECm ki\u1EBFm v\xE0 thay\
  \ th\u1EBF v\u0103n b\u1EA3n b\u1EB1ng Haskell. Ch\xFAng ta s\u1EBD s\u1EED d\u1EE5\
  ng `Data.Text` cho vi\u1EC7c x\u1EED l\xFD v\u0103n b\u1EA3n Unicode v\xE0 hi\u1EC7\
  u qu\u1EA3. H\xE3y ch\u1EAFc\u2026"
lastmod: '2024-03-13T22:44:36.693077-06:00'
model: gpt-4-0125-preview
summary: "Ch\xFAng ta h\xE3y t\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3\
  n b\u1EB1ng Haskell."
title: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n"
weight: 10
---

## Làm thế nào:
Chúng ta hãy tìm kiếm và thay thế văn bản bằng Haskell. Chúng ta sẽ sử dụng `Data.Text` cho việc xử lý văn bản Unicode và hiệu quả. Hãy chắc chắn nhập khẩu `Data.Text` như sau:

```haskell
import qualified Data.Text as T
```

Bây giờ, hãy thay thế tất cả các trường hợp của "hello" bằng "hi" trong một văn bản:

```haskell
replaceText :: T.Text -> T.Text -> T.Text -> T.Text
replaceText old new = T.replace old new

main :: IO ()
main = do
  let originalText = T.pack "hello world, hello Haskell!"
  let newText = replaceText (T.pack "hello") (T.pack "hi") originalText
  print newText -- "hi world, hi Haskell!"
```

Hàm `replace` thực hiện phần nặng nhọc. Chúng tôi đã bao bọc nó trong `replaceText` để làm rõ hơn.

## Sâu hơn
Các hàm thay thế văn bản của Haskell như `T.replace` được xây dựng trên khả năng xử lý mảng của Haskell. Nhìn lại, Haskell được tưởng tượng ra vào những năm '80, với trọng tâm vào lập trình chức năng. Paradigm này làm cho những thao tác như thay thế văn bản trở nên tinh tế và ít lỗi hơn do tính bất biến và hệ thống kiểu mạnh.

Đối với các phương án thay thế, bạn có thể tự lặp qua văn bản và thay thế các chuỗi con, nhưng điều đó sẽ gặp nhiều lỗi hơn và kém hiệu quả.

Thư viện `Data.Text` sử dụng một biểu diễn nội bộ khác so với loại `String` (chỉ là một danh sách các ký tự), làm cho nó phù hợp hơn cho các hoạt động văn bản quy mô lớn. Chính hàm `T.replace` sử dụng các thuật toán hiệu quả cho việc tìm kiếm chuỗi, mang lại hiệu suất tốt ngay cả với văn bản lớn.

## Xem thêm
Để tìm hiểu thêm về `Data.Text`, hãy xem:

- [Gói Text trên Hackage](https://hackage.haskell.org/package/text)

Cũng cân nhắc việc đọc rộng rãi hơn về việc điều chỉnh chuỗi trong Haskell:

- [Haskell Wiki về chuỗi](https://wiki.haskell.org/Strings)
- [Học Haskell Cho Việc Vui Vẻ và Lợi Ích! về Văn bản](http://learnyouahaskell.com/input-and-output#files-and-streams)
