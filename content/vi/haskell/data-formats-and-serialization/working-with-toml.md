---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:31.123791-07:00
description: "C\xE1i g\xEC & T\u1EA1i sao? L\xE0m vi\u1EC7c v\u1EDBi TOML bao g\u1ED3\
  m vi\u1EC7c ph\xE2n t\xEDch c\xFA ph\xE1p v\xE0 t\u1EA1o ra d\u1EEF li\u1EC7u TOML\
  \ (Tom's Obvious, Minimal Language - Ng\xF4n ng\u1EEF T\u1ED1i gi\u1EA3n, R\xF5\
  \ r\xE0ng\u2026"
lastmod: '2024-04-05T21:53:38.126518-06:00'
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi TOML bao g\u1ED3m vi\u1EC7c ph\xE2n t\xEDch c\xFA\
  \ ph\xE1p v\xE0 t\u1EA1o ra d\u1EEF li\u1EC7u TOML (Tom's Obvious, Minimal Language\
  \ - Ng\xF4n ng\u1EEF T\u1ED1i gi\u1EA3n, R\xF5 r\xE0ng c\u1EE7a Tom) b\u1EB1ng Haskell."
title: "L\xE0m vi\u1EC7c v\u1EDBi TOML"
weight: 39
---

### Cái gì & Tại sao?
Làm việc với TOML bao gồm việc phân tích cú pháp và tạo ra dữ liệu TOML (Tom's Obvious, Minimal Language - Ngôn ngữ Tối giản, Rõ ràng của Tom) bằng Haskell. Các lập trình viên thực hiện điều này để dễ dàng quản lý các tệp cấu hình hoặc trao đổi dữ liệu với đảm bảo kiểu mạnh mẽ và cú pháp tối thiểu.

### Làm thế nào:
Đầu tiên, hãy đảm bảo bạn có một thư viện phân tích cú pháp TOML. Đối với Haskell, `htoml` là một lựa chọn phổ biến. Bạn cần thêm nó vào các phụ thuộc dự án của mình.

```Haskell
-- Nhập khẩu thư viện phân tích cú pháp TOML
import qualified Text.Toml as Toml

-- Định nghĩa cấu trúc dữ liệu cấu hình của bạn
data Config = Config {
  title :: String,
  owner :: Owner
} deriving (Show)

data Owner = Owner {
  name :: String,
  dob :: Maybe Day -- Ngày tùy chọn
} deriving (Show)

-- Phân tích cú pháp một chuỗi TOML
main :: IO ()
main = do
  let tomlData = "[owner]\nname = \"Tom Preston-Werner\"\ndob = 1979-05-27T07:32:00Z"
  case Toml.parseTomlDoc "" tomlData of
    Left err -> putStrLn $ "Lỗi: " ++ show err
    Right toml -> print toml -- Hoặc xử lý thêm TOML đã phân tích
```

Kết quả mẫu có thể được cấu trúc và truy cập như bất kỳ kiểu dữ liệu Haskell nào.

### Tìm hiểu sâu
Trong lịch sử, TOML được tạo ra bởi Tom Preston-Werner, đồng sáng lập GitHub, như một phản ứng với độ phức tạp của YAML và JSON đối với các tệp cấu hình. Nó nhấn mạnh việc dễ đọc và dễ viết hơn JSON, và nghiêm ngặt và đơn giản hơn YAML.

Các lựa chọn thay thế cho TOML bao gồm JSON và YAML, với mỗi định dạng có những điểm mạnh riêng. JSON là phổ biến và không phụ thuộc vào ngôn ngữ, trong khi YAML cung cấp một định dạng dễ đọc hơn cho con người. TOML được đánh giá cao vì sự đơn giản và nhất quán của nó, tránh một số điểm yếu của các định dạng liên quan.

Triển khai trong Haskell thông thường liên quan đến một thư viện phân tích cú pháp TOML thành một kiểu dữ liệu Haskell, thường tận dụng hệ thống kiểu tiên tiến của Haskell để đảm bảo tính chính xác. Phân tích cú pháp có thể được thực hiện thông qua phương pháp đệ quy hoặc phân tích kết hợp, cân bằng hiệu quả với khả năng đọc và bảo trì của mã.

### Xem thêm
- `htoml`: https://hackage.haskell.org/package/htoml
- Kho lưu trữ GitHub chính thức của TOML: https://github.com/toml-lang/toml
- So sánh các định dạng tuần tự hóa dữ liệu: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
