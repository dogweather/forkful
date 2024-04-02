---
date: 2024-01-26 04:45:46.648275-07:00
description: "\uBCF5\uC18C\uC218\uB294 \uC2E4\uC218 \uBD80\uBD84\uACFC \uD5C8\uC218\
  \ \uBD80\uBD84(\uC608: 3 + 4i)\uC744 \uAC00\uC9C0\uACE0 \uC788\uC2B5\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 Swift\uC5D0\uC11C \uC2E0\uD638 \uCC98\
  \uB9AC, \uD2B9\uC815 \uC218\uD559 \uBB38\uC81C \uD574\uACB0, \uBB3C\uB9AC \uC2DC\
  \uBBAC\uB808\uC774\uC158 \uB4F1\uC758 \uC791\uC5C5\uC5D0 \uBCF5\uC18C\uC218\uB97C\
  \ \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.722902-06:00'
model: gpt-4-0125-preview
summary: "\uBCF5\uC18C\uC218\uB294 \uC2E4\uC218 \uBD80\uBD84\uACFC \uD5C8\uC218 \uBD80\
  \uBD84(\uC608: 3 + 4i)\uC744 \uAC00\uC9C0\uACE0 \uC788\uC2B5\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 Swift\uC5D0\uC11C \uC2E0\uD638 \uCC98\uB9AC\
  , \uD2B9\uC815 \uC218\uD559 \uBB38\uC81C \uD574\uACB0, \uBB3C\uB9AC \uC2DC\uBBAC\
  \uB808\uC774\uC158 \uB4F1\uC758 \uC791\uC5C5\uC5D0 \uBCF5\uC18C\uC218\uB97C \uC0AC\
  \uC6A9\uD569\uB2C8\uB2E4."
title: "\uBCF5\uC18C\uC218 \uB2E4\uB8E8\uAE30"
weight: 14
---

## 무엇인가 & 왜?
복소수는 실수 부분과 허수 부분(예: 3 + 4i)을 가지고 있습니다. 프로그래머들은 Swift에서 신호 처리, 특정 수학 문제 해결, 물리 시뮬레이션 등의 작업에 복소수를 사용합니다.

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
