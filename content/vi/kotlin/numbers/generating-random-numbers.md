---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:46.496413-07:00
description: "Vi\u1EC7c t\u1EA1o ra c\xE1c s\u1ED1 ng\u1EABu nhi\xEAn trong l\u1EAD\
  p tr\xECnh l\xE0 v\u1EC1 vi\u1EC7c t\u1EA1o ra nh\u1EEFng con s\u1ED1 kh\xF4ng c\xF3\
  \ b\u1EA5t k\u1EF3 m\u1ED9t m\u1EABu \u0111o\xE1n tr\u01B0\u1EDBc n\xE0o. C\xE1\
  c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y\u2026"
lastmod: 2024-02-19 22:04:55.765854
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\u1EA1o ra c\xE1c s\u1ED1 ng\u1EABu nhi\xEAn trong l\u1EADp tr\xEC\
  nh l\xE0 v\u1EC1 vi\u1EC7c t\u1EA1o ra nh\u1EEFng con s\u1ED1 kh\xF4ng c\xF3 b\u1EA5\
  t k\u1EF3 m\u1ED9t m\u1EABu \u0111o\xE1n tr\u01B0\u1EDBc n\xE0o. C\xE1c l\u1EAD\
  p tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y\u2026"
title: "Sinh s\u1ED1 ng\u1EABu nhi\xEAn"
---

{{< edit_this_page >}}

## Gì & Tại sao?

Việc tạo ra các số ngẫu nhiên trong lập trình là về việc tạo ra những con số không có bất kỳ một mẫu đoán trước nào. Các lập trình viên thực hiện điều này với nhiều lý do khác nhau, bao gồm mô phỏng, kiểm thử thuật toán, trò chơi, và các ứng dụng bảo mật, nơi sự không dự đoán được là chìa khóa để đạt được kết quả thực tế hoặc an toàn.

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
