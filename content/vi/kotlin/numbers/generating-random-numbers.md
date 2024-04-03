---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:46.496413-07:00
description: "L\xE0m th\u1EBF n\xE0o: Kotlin cung c\u1EA5p m\u1ED9t c\xE1ch tr\u1EF1\
  c ti\u1EBFp \u0111\u1EC3 t\u1EA1o ra c\xE1c s\u1ED1 ng\u1EABu nhi\xEAn qua th\u01B0\
  \ vi\u1EC7n chu\u1EA9n c\u1EE7a m\xECnh. D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1\
  ch b\u1EA1n c\xF3 th\u1EC3 t\u1EA1o ra c\xE1c lo\u1EA1i gi\xE1\u2026"
lastmod: '2024-03-13T22:44:36.596115-06:00'
model: gpt-4-0125-preview
summary: "Kotlin cung c\u1EA5p m\u1ED9t c\xE1ch tr\u1EF1c ti\u1EBFp \u0111\u1EC3 t\u1EA1\
  o ra c\xE1c s\u1ED1 ng\u1EABu nhi\xEAn qua th\u01B0 vi\u1EC7n chu\u1EA9n c\u1EE7\
  a m\xECnh."
title: "Sinh s\u1ED1 ng\u1EABu nhi\xEAn"
weight: 12
---

## Làm thế nào:
Kotlin cung cấp một cách trực tiếp để tạo ra các số ngẫu nhiên qua thư viện chuẩn của mình. Dưới đây là cách bạn có thể tạo ra các loại giá trị ngẫu nhiên khác nhau:

### Tạo một Số Nguyên Ngẫu Nhiên
Để tạo một số nguyên ngẫu nhiên trong một phạm vi cụ thể:

```kotlin
import kotlin.random.Random

fun main() {
    val randomNumber = Random.nextInt(1, 100) // Tạo một số ngẫu nhiên giữa 1 và 99
    println(randomNumber)
}
```

### Tạo một Số Thực Kép Ngẫu Nhiên
Tương tự, để tạo một số thực kép ngẫu nhiên:

```kotlin
import kotlin.random.Random

fun main() {
    val randomDouble = Random.nextDouble(1.0, 10.0) // Tạo một số thực kép ngẫu nhiên giữa 1.0 và 10.0
    println(randomDouble)
}
```

### Tạo một Giá Trị Boolean Ngẫu Nhiên
Để tạo một giá trị boolean ngẫu nhiên:

```kotlin
import kotlin.random.Random

fun main() {
    val randomBoolean = Random.nextBoolean() // Tạo ngẫu nhiên giá trị true hoặc false
    println(randomBoolean)
}
```

### Gieo Hạt để Có Kết Quả Có Thể Tái Tạo
Trong trường hợp bạn cần một chuỗi các số ngẫu nhiên có thể tái tạo (ví dụ, trong kiểm thử), bạn có thể gieo hạt cho trình sinh số ngẫu nhiên:

```kotlin
import kotlin.random.Random

fun main() {
    val seed = 12345L
    val random = Random(seed)
    val randomNumber = random.nextInt(1, 100)
    println(randomNumber)
}
```

## Thảo Luận Sâu hơn
Cách tiếp cận của thư viện chuẩn Kotlin trong việc tạo ra các số ngẫu nhiên sử dụng `java.util.Random` bên dưới, đảm bảo sự hòa trộn giữa sự tiện dụng và hiệu suất. Tuy nhiên, điều quan trọng cần lưu ý là các phương thức này tạo ra các số ngẫu nhiên giả, tức là các số này trông có vẻ ngẫu nhiên nhưng thực sự được tạo ra nhờ quá trình xác định.

Đối với hầu hết các ứng dụng, sự ngẫu nhiên được cung cấp bởi lớp `Random` của Kotlin là đủ. Tuy nhiên, đối với các ứng dụng nhạy cảm về mặt bảo mật hơn, như mật mã học, nơi chất lượng của sự ngẫu nhiên là quan trọng nhất, người ta nên cân nhắc sử dụng `java.security.SecureRandom` thay thế. SecureRandom được thiết kế riêng cho các hoạt động mật mã, cung cấp chất lượng ngẫu nhiên cao hơn, mặc dù có thể có sự đánh đổi về hiệu suất.

Kotlin không tái sáng tạo lốp xe nhưng cung cấp một API của Kotlin thân thiện qua các cơ chế sinh số ngẫu nhiên của Java, khiến việc sử dụng trong các dự án Kotlin trở nên dễ dàng hơn và gọn gàng hơn. Như mọi khi, khi xử lý vấn đề ngẫu nhiên, lập trình viên nên cân nhắc cẩn thận về trường hợp sử dụng để chọn công cụ phù hợp nhất cho công việc.
