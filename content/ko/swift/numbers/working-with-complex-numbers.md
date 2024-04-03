---
date: 2024-01-26 04:45:46.648275-07:00
description: "\uBC29\uBC95: Swift\uC5D0\uB294 \uB0B4\uC7A5 \uBCF5\uC18C\uC218 \uC9C0\
  \uC6D0\uC774 \uC5C6\uC9C0\uB9CC, \uC6B0\uB9AC \uC2A4\uC2A4\uB85C \uB9CC\uB4E4 \uC218\
  \ \uC788\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.722902-06:00'
model: gpt-4-0125-preview
summary: "Swift\uC5D0\uB294 \uB0B4\uC7A5 \uBCF5\uC18C\uC218 \uC9C0\uC6D0\uC774 \uC5C6\
  \uC9C0\uB9CC, \uC6B0\uB9AC \uC2A4\uC2A4\uB85C \uB9CC\uB4E4 \uC218 \uC788\uC2B5\uB2C8\
  \uB2E4."
title: "\uBCF5\uC18C\uC218 \uB2E4\uB8E8\uAE30"
weight: 14
---

## 방법:
Swift에는 내장 복소수 지원이 없지만, 우리 스스로 만들 수 있습니다:

```Swift
struct ComplexNumber {
    var real: Double
    var imaginary: Double
    
    func add(_ other: ComplexNumber) -> ComplexNumber {
        return ComplexNumber(real: real + other.real, imaginary: imaginary + other.imaginary)
    }
    
    // 뺄셈, 곱셈 등의 추가 메소드
}

let first = ComplexNumber(real: 2, imaginary: 3)
let second = ComplexNumber(real: 1, imaginary: 4)
let result = first.add(second)
print("결과: \(result.real) + \(result.imaginary)i")
// 샘플 출력: 결과: 3.0 + 7.0i
```

## 심층 탐구
복소수는 16세기 대수 방정식에서 등장하였습니다. 그것들은 양자역학, 제어 이론, 그리고 다른 많은 분야에서 필수적입니다. Apple의 Swift는 Python이나 C++과 같은 언어처럼 복소수를 위한 표준 라이브러리가 없습니다. 스스로 만드는 것 외의 대안으로는 복소수 지원이 포함된 숫자 패키지를 사용하거나 Swift의 상호 운용성으로 C++ 복소수 라이브러리를 래핑하는 것이 있습니다.

## 참고하십시오
- Swift Numerics: [https://github.com/apple/swift-numerics](https://github.com/apple/swift-numerics)
