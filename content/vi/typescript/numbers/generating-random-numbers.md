---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:38.969993-07:00
description: "Vi\u1EC7c t\u1EA1o s\u1ED1 ng\u1EABu nhi\xEAn trong TypeScript li\xEA\
  n quan \u0111\u1EBFn vi\u1EC7c t\u1EA1o ra c\xE1c gi\xE1 tr\u1ECB s\u1ED1 kh\xF4\
  ng th\u1EC3 \u0111o\xE1n tr\u01B0\u1EDBc trong m\u1ED9t ph\u1EA1m vi \u0111\xE3\
  \ x\xE1c \u0111\u1ECBnh. L\u1EADp tr\xECnh vi\xEAn t\u1EADn\u2026"
lastmod: '2024-02-25T18:49:34.638853-07:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\u1EA1o s\u1ED1 ng\u1EABu nhi\xEAn trong TypeScript li\xEAn quan\
  \ \u0111\u1EBFn vi\u1EC7c t\u1EA1o ra c\xE1c gi\xE1 tr\u1ECB s\u1ED1 kh\xF4ng th\u1EC3\
  \ \u0111o\xE1n tr\u01B0\u1EDBc trong m\u1ED9t ph\u1EA1m vi \u0111\xE3 x\xE1c \u0111\
  \u1ECBnh. L\u1EADp tr\xECnh vi\xEAn t\u1EADn\u2026"
title: "Sinh s\u1ED1 ng\u1EABu nhi\xEAn"
---

{{< edit_this_page >}}

## Điều gì và Tại sao?

Việc tạo số ngẫu nhiên trong TypeScript liên quan đến việc tạo ra các giá trị số không thể đoán trước trong một phạm vi đã xác định. Lập trình viên tận dụng những con số ngẫu nhiên này cho nhiều mục đích, như tạo các định danh duy nhất, mô phỏng dữ liệu để kiểm tra, hoặc thêm tính không dự đoán vào trò chơi và các mô phỏng.

## Cách thực hiện:

Trong TypeScript, bạn có thể tạo số ngẫu nhiên sử dụng đối tượng `Math` toàn cục. Dưới đây là một số ví dụ thực tế minh họa cách tạo ra số ngẫu nhiên cho các nhu cầu khác nhau.

### Tạo một Số Ngẫu nhiên Cơ bản

Để tạo một số thập phân ngẫu nhiên cơ bản giữa 0 (bao gồm) và 1 (không bao gồm), bạn sử dụng `Math.random()`. Điều này không yêu cầu bất kỳ thao tác bổ sung nào:

```TypeScript
const randomNumber = Math.random();
console.log(randomNumber);
```

Điều này có thể xuất ra một giá trị như `0.8995452185604771`.

### Tạo một Số Nguyên Ngẫu nhiên Giữa Hai Giá trị

Khi bạn cần một số nguyên giữa hai giá trị cụ thể, bạn kết hợp cả `Math.random()` và một số phép tính:

```TypeScript
function getRandomInt(min: number, max: number): number {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

const randomInt = getRandomInt(1, 10);
console.log(randomInt);
```

Điều này có thể xuất ra một giá trị số nguyên giữa 1 và 10, chẳng hạn như `7`.

### Tạo một Định danh Duy nhất

Số ngẫu nhiên có thể được kết hợp với các phương pháp khác để tạo ra các định danh duy nhất, ví dụ như một đoạn mã sinh UUID đơn giản:

```TypeScript
function generateUUID(): string {
    return 'xxxxyxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
        const r = Math.random() * 16 | 0, v = c == 'x' ? r : (r & 0x3 | 0x8);
        return v.toString(16);
    });
}

const uuid = generateUUID();
console.log(uuid);
```

Điều này tạo ra một chuỗi giống như UUID, như là `110e8400-e29b-41d4-a716-446655440000`.

## Nghiên cứu Sâu hơn

Phương pháp chính để tạo số ngẫu nhiên trong JavaScript và do đó trong TypeScript, `Math.random()`, dựa trên một bộ sinh số ngẫu nhiên giả (PRNG). Điều quan trọng cần lưu ý là mặc dù kết quả có vẻ ngẫu nhiên, chúng được tạo ra bởi một thuật toán xác định dựa trên một giá trị hạt giống ban đầu. Do đó, các số được tạo bởi `Math.random()` không phải là thực sự ngẫu nhiên và không nên được sử dụng cho mục đích mật mã hóa.

Đối với các số ngẫu nhiên an toàn về mật mã, Web Crypto API cung cấp `crypto.getRandomValues()`, có thể truy cập trong các môi trường hỗ trợ tiêu chuẩn Web Crypto, bao gồm các trình duyệt hiện đại và Node.js (thông qua mô-đun `crypto`). Dưới đây là một ví dụ nhanh minh họa cách sử dụng nó trong TypeScript để tạo một số ngẫu nhiên an toàn trong một phạm vi:

```TypeScript
function secureRandom(min: number, max: number): number {
    const array = new Uint32Array(1);
    window.crypto.getRandomValues(array);
    return min + (array[0] % (max - min + 1));
}

const secureRandNum = secureRandom(1, 100);
console.log(secureRandNum);
```

Phương pháp này cung cấp một mức độ ngẫu nhiên mạnh mẽ hơn và phù hợp hơn cho các ứng dụng nhạy cảm với bảo mật. Tuy nhiên, nó cũng tiêu tốn nhiều tài nguyên hơn và có thể không cần thiết cho các nhiệm vụ bình thường hơn, như mô phỏng đơn giản hoặc tạo giá trị ngẫu nhiên không quan trọng.
