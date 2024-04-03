---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:39.332731-07:00
description: "L\xE0m th\u1EBF n\xE0o: Swift cung c\u1EA5p m\u1ED9t c\xE1ch \u0111\u01A1\
  n gi\u1EA3n \u0111\u1EC3 t\u1EA1o s\u1ED1 ng\u1EABu nhi\xEAn th\xF4ng qua th\u01B0\
  \ vi\u1EC7n ti\xEAu chu\u1EA9n c\u1EE7a n\xF3. D\u01B0\u1EDBi \u0111\xE2y l\xE0\
  \ c\xE1ch b\u1EA1n th\u1EF1c hi\u1EC7n cho c\xE1c lo\u1EA1i s\u1ED1\u2026"
lastmod: '2024-03-13T22:44:37.091225-06:00'
model: gpt-4-0125-preview
summary: "Swift cung c\u1EA5p m\u1ED9t c\xE1ch \u0111\u01A1n gi\u1EA3n \u0111\u1EC3\
  \ t\u1EA1o s\u1ED1 ng\u1EABu nhi\xEAn th\xF4ng qua th\u01B0 vi\u1EC7n ti\xEAu chu\u1EA9\
  n c\u1EE7a n\xF3."
title: "Sinh s\u1ED1 ng\u1EABu nhi\xEAn"
weight: 12
---

## Làm thế nào:
Swift cung cấp một cách đơn giản để tạo số ngẫu nhiên thông qua thư viện tiêu chuẩn của nó. Dưới đây là cách bạn thực hiện cho các loại số khác nhau:

```Swift
// Tạo một số nguyên ngẫu nhiên từ 0 đến Int.max
let randomInt = Int.random(in: 0...Int.max)
print(randomInt)

// Tạo một số thực ngẫu nhiên từ 0.0 đến 1.0
let randomDouble = Double.random(in: 0.0...1.0)
print(randomDouble)

// Tạo một giá trị Bool ngẫu nhiên
let randomBool = Bool.random()
print(randomBool)
```

Kết quả mẫu có thể biến đổi vì, cuối cùng, chúng ta đang xử lý sự ngẫu nhiên. Chạy mã nhiều lần sẽ cho ra các số và giá trị boolean khác nhau.

## Đào sâu
Cách tiếp cận của Swift đối với việc tạo số ngẫu nhiên được xây dựng dựa trên một bộ sinh số ngẫu nhiên giả (PRNG) vững chắc và hiệu quả. Trước Swift 4.2, các nhà phát triển phụ thuộc vào thư viện bên ngoài hoặc khả năng của nền tảng cơ bản, có thể dẫn đến sự không nhất quán trên các nền tảng và môi trường khác nhau. Với sự giới thiệu về API bản địa trong Swift 4.2, việc tạo số ngẫu nhiên trở nên đơn giản hơn và nhất quán hơn, bất kể nền tảng cơ bản là gì.

Tuy nhiên, điều quan trọng là phải hiểu rằng bộ sinh số ngẫu nhiên tiêu chuẩn trong Swift không phù hợp cho mục đích mã hóa. Đối với mục đích mã hóa, các nhà phát triển nên sử dụng khung `Security` trên các nền tảng của Apple, cung cấp quyền truy cập vào các byte ngẫu nhiên an toàn từ góc độ mã hóa. Tính đến lần cập nhật cuối cùng của tôi, Swift không bao gồm một bộ sinh số ngẫu nhiên mã hóa đa nền tảng trong thư viện tiêu chuẩn của mình, buộc các nhà phát triển tìm đến các thư viện bên thứ ba cho những nhu cầu như vậy trên các nền tảng không phải Apple.

Trong lĩnh vực tính toán khoa học hoặc các tình huống yêu cầu một chuỗi số ngẫu nhiên giả có thể xác định (nghĩa là chuỗi có thể được tái tạo một cách chính xác), việc tạo số ngẫu nhiên của Swift có thể không phải là lựa chọn tốt nhất nếu không có khả năng gieo mầm cho bộ sinh. Trong những trường hợp như vậy, thường phải sử dụng các thư viện và thuật toán chuyên biệt để đáp ứng những yêu cầu chính xác này.
