---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:39.840689-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Java, vi\u1EC7c t\u1EA1o s\u1ED1 ng\u1EAB\
  u nhi\xEAn c\xF3 th\u1EC3 \u0111\u01B0\u1EE3c th\u1EF1c hi\u1EC7n b\u1EB1ng c\xE1\
  ch s\u1EED d\u1EE5ng l\u1EDBp `Random` t\u1EEB g\xF3i `java.util`, ho\u1EB7c c\xE1\
  c l\u1EDBp `ThreadLocalRandom`\u2026"
lastmod: '2024-03-13T22:44:36.483808-06:00'
model: gpt-4-0125-preview
summary: "Trong Java, vi\u1EC7c t\u1EA1o s\u1ED1 ng\u1EABu nhi\xEAn c\xF3 th\u1EC3\
  \ \u0111\u01B0\u1EE3c th\u1EF1c hi\u1EC7n b\u1EB1ng c\xE1ch s\u1EED d\u1EE5ng l\u1EDB\
  p `Random` t\u1EEB g\xF3i `java.util`, ho\u1EB7c c\xE1c l\u1EDBp `ThreadLocalRandom`\
  \ v\xE0 `SecureRandom` cho c\xE1c tr\u01B0\u1EDDng h\u1EE3p s\u1EED d\u1EE5ng c\u1EE5\
  \ th\u1EC3."
title: "Sinh s\u1ED1 ng\u1EABu nhi\xEAn"
weight: 12
---

## Làm thế nào:
Trong Java, việc tạo số ngẫu nhiên có thể được thực hiện bằng cách sử dụng lớp `Random` từ gói `java.util`, hoặc các lớp `ThreadLocalRandom` và `SecureRandom` cho các trường hợp sử dụng cụ thể. Các ví dụ dưới đây minh họa cách sử dụng các lớp này.

### Sử dụng lớp `Random`
Lớp `Random` cung cấp một cách để tạo ra những số ngẫu nhiên giả mạo đơn giản.

```Java
import java.util.Random;

public class RandomExample {
    public static void main(String[] args) {
        Random rand = new Random(); // Tạo một đối tượng Random

        int randInt = rand.nextInt(50); // Tạo một số nguyên ngẫu nhiên từ 0 đến 49
        double randDouble = rand.nextDouble(); // Tạo một số double ngẫu nhiên giữa 0.0 và 1.0
        boolean randBoolean = rand.nextBoolean(); // Tạo một boolean ngẫu nhiên
        
        System.out.println("Số nguyên ngẫu nhiên: " + randInt);
        System.out.println("Số double ngẫu nhiên: " + randDouble);
        System.out.println("Boolean ngẫu nhiên: " + randBoolean);
    }
}
```

### Sử dụng lớp `ThreadLocalRandom`
Đối với các ứng dụng đồng thời, `ThreadLocalRandom` hiệu quả hơn `Random`.

```Java
import java.util.concurrent.ThreadLocalRandom;

public class ThreadLocalRandomExample {
    public static void main(String[] args) {
        int randInt = ThreadLocalRandom.current().nextInt(1, 101); // Từ 1 đến 100
        double randDouble = ThreadLocalRandom.current().nextDouble(1.0, 10.0); // Từ 1.0 đến 10.0
        
        System.out.println("Số nguyên ngẫu nhiên: " + randInt);
        System.out.println("Số double ngẫu nhiên: " + randDouble);
    }
}
```

### Sử dụng lớp `SecureRandom`
Đối với các hoạt động mật mã, `SecureRandom` cung cấp một mức độ an ninh cao hơn.

```Java
import java.security.SecureRandom;

public class SecureRandomExample {
    public static void main(String[] args) {
        SecureRandom secRand = new SecureRandom();
        
        byte[] bytes = new byte[20];
        secRand.nextBytes(bytes); // Đổ đầy bytes bằng số ngẫu nhiên an toàn
        
        System.out.println("Các byte ngẫu nhiên an toàn:");
        for (byte b : bytes) {
            System.out.printf("%02x ", b);
        }
    }
}
```

## Sâu hơn
Việc tạo số ngẫu nhiên đã phát triển đáng kể kể từ những ngày đầu của công nghệ thông tin. Lớp `Random` của Java sử dụng công thức tuyến tính đồng nhất để tạo ra các số ngẫu nhiên giả mạo, chúng có tính chất xác định và không phù hợp cho các ứng dụng có tính bảo mật cao. Điều này đã dẫn đến việc giới thiệu `SecureRandom`, sử dụng các thuật toán phức tạp hơn (ví dụ: SHA1PRNG) để sản xuất số ngẫu nhiên mạnh mẽ về mặt mật mã.

Tuy nhiên, `Random` và `SecureRandom` có nhược điểm của chúng, chẳng hạn như suy giảm hiệu suất trong môi trường đa luồng. Lớp `ThreadLocalRandom` được giới thiệu trong Java 7 để giải quyết vấn đề này bằng cách cung cấp các bộ sinh số ngẫu nhiên cục bộ cho mỗi luồng, cải thiện đáng kể hiệu suất trong các ứng dụng đồng thời.

Mặc dù các lớp này đáp ứng hầu hết nhu cầu, nhưng đối với các yêu cầu chuyên biệt hoặc quy mô cực lớn, các nhà phát triển có thể khám phá thêm các thư viện bổ sung hoặc phát triển các giải pháp tùy chỉnh. Điều quan trọng là phải chọn cách tiếp cận phù hợp dựa trên nhu cầu bảo mật và yêu cầu về hiệu suất của trường hợp sử dụng.
