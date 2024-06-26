---
date: 2024-01-26 04:42:11.013235-07:00
description: "\uC5B4\uB5BB\uAC8C \uD558\uB098: Java\uB294 \uBCF5\uC18C\uC218\uB97C\
  \ \uB0B4\uC7A5 \uC9C0\uC6D0\uD558\uC9C0 \uC54A\uC9C0\uB9CC, \uC790\uCCB4 \uD074\uB798\
  \uC2A4\uB97C \uB9CC\uB4E4\uAC70\uB098 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\
  \uC6A9\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC5EC\uAE30 \uAC04\uB2E8\uD55C `ComplexNumber`\
  \ \uD074\uB798\uC2A4\uB97C \uB9CC\uB4E4\uACE0 \uC0AC\uC6A9\uD558\uB294 \uBC29\uBC95\
  \uC758 \uC608\uC2DC\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.038774-06:00'
model: gpt-4-0125-preview
summary: "Java\uB294 \uBCF5\uC18C\uC218\uB97C \uB0B4\uC7A5 \uC9C0\uC6D0\uD558\uC9C0\
  \ \uC54A\uC9C0\uB9CC, \uC790\uCCB4 \uD074\uB798\uC2A4\uB97C \uB9CC\uB4E4\uAC70\uB098\
  \ \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD560 \uC218 \uC788\uC2B5\uB2C8\
  \uB2E4."
title: "\uBCF5\uC18C\uC218 \uB2E4\uB8E8\uAE30"
weight: 14
---

## 어떻게 하나:
Java는 복소수를 내장 지원하지 않지만, 자체 클래스를 만들거나 라이브러리를 사용할 수 있습니다. 여기 간단한 `ComplexNumber` 클래스를 만들고 사용하는 방법의 예시입니다:

```java
public class ComplexNumber {
    private double real;
    private double imaginary;

    public ComplexNumber(double real, double imaginary) {
        this.real = real;
        this.imaginary = imaginary;
    }

    public ComplexNumber add(ComplexNumber other) {
        return new ComplexNumber(this.real + other.real, this.imaginary + other.imaginary);
    }

    // 복소수를 a + bi 형식으로 표시하는 ToString
    @Override
    public String toString() {
        return String.format("%.1f + %.1fi", real, imaginary);
    }

    // 신속한 테스트
    public static void main(String[] args) {
        ComplexNumber c1 = new ComplexNumber(2, 3);
        ComplexNumber c2 = new ComplexNumber(1, 4);

        System.out.println("합: " + c1.add(c2));
    }
}
```

main 메서드의 샘플 출력 결과는 다음과 같습니다:

```
합: 3.0 + 7.0i
```

## 심층 탐구
Java와 같은 고급 언어가 나오기 전에는, 프로그래머들이 Fortran이나 C와 같은 언어에서 직접 수학 라이브러리를 사용하여 복잡한 연산을 관리했습니다. 이 개념은 16세기에 거슬러 올라가며, Gerolamo Cardano와 Rafael Bombelli와 같은 수학자들에게 그 공로가 돌아갑니다.

Java에서는 `java.lang.Math`가 기본으로 가지만 복소수는 제외됩니다. 아마도 모든 프로그래머가 사용하지 않기 때문일 것입니다. 대안? 라이브러리를 사용하세요. Apache Commons Math는 조작을 위한 메서드로 가득 찬 `Complex` 클래스를 제공합니다. 그러나 자체적으로 롤링하는 것이 멋진 이유는 다음과 같습니다: 가볍고, 정확히 필요한 것에 맞춤화되며, 라이브러리 오버헤드가 없습니다.

중요한 세부 사항 하나: 부동소수점 정밀도에 주의하세요. 컴퓨터는 일부 숫자를 정확하게 표현할 수 없어 반올림 오류가 발생할 수 있습니다. 반복적인 복잡한 연산을 수행할 때, 이러한 오류가 누적될 수 있습니다!

## 참조
더 깊은 탐구와 더 복잡한 연산을 확인하려면 다음을 확인하세요:

- [Apache Commons Math](https://commons.apache.org/proper/commons-math/)
- [JScience의 Complex 클래스](http://jscience.org/)
- Oracle의 [부동소수점 산술](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)에 관한 튜토리얼
