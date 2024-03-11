---
date: 2024-01-26 03:45:44.979286-07:00
description: "\uC22B\uC790\uB97C \uBC18\uC62C\uB9BC\uD55C\uB2E4\uB294 \uAC83\uC740\
  \ \uC9C0\uC815\uB41C \uC815\uBC00\uB3C4\uB85C \uC870\uC815\uD55C\uB2E4\uB294 \uAC83\
  \uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uAC00\uB3C5\uC131\uC744 \uC704\uD574 \uC22B\uC790\uB97C \uB2E8\uC21C\uD654\uD558\
  \uAC70\uB098, \uD2B9\uC815 \uC0AC\uC591\uC744 \uCDA9\uC871\uC2DC\uD0A4\uAC70\uB098\
  , \uD2B9\uC815 \uBC94\uC704 \uB0B4\uC5D0\uC11C \uACC4\uC0B0\uC774 \uC77C\uCE58\uD558\
  \uB3C4\uB85D \uD558\uAE30 \uC704\uD574 (\uC608: \uBD80\uB3D9 \uC18C\uC218\uC810\
  \ \uC0B0\uC220\uC5D0\uC11C\uC758 \uC815\uBC00\uB3C4 \uC624\uB958\uB97C \uD53C\uD558\
  \uAE30 \uC704\uD574) \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD569\uB2C8\uB2E4."
lastmod: '2024-03-11T00:14:28.959905-06:00'
model: gpt-4-0125-preview
summary: "\uC22B\uC790\uB97C \uBC18\uC62C\uB9BC\uD55C\uB2E4\uB294 \uAC83\uC740 \uC9C0\
  \uC815\uB41C \uC815\uBC00\uB3C4\uB85C \uC870\uC815\uD55C\uB2E4\uB294 \uAC83\uC744\
  \ \uC758\uBBF8\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uAC00\
  \uB3C5\uC131\uC744 \uC704\uD574 \uC22B\uC790\uB97C \uB2E8\uC21C\uD654\uD558\uAC70\
  \uB098, \uD2B9\uC815 \uC0AC\uC591\uC744 \uCDA9\uC871\uC2DC\uD0A4\uAC70\uB098, \uD2B9\
  \uC815 \uBC94\uC704 \uB0B4\uC5D0\uC11C \uACC4\uC0B0\uC774 \uC77C\uCE58\uD558\uB3C4\
  \uB85D \uD558\uAE30 \uC704\uD574 (\uC608: \uBD80\uB3D9 \uC18C\uC218\uC810 \uC0B0\
  \uC220\uC5D0\uC11C\uC758 \uC815\uBC00\uB3C4 \uC624\uB958\uB97C \uD53C\uD558\uAE30\
  \ \uC704\uD574) \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD569\uB2C8\uB2E4."
title: "\uC22B\uC790 \uBC18\uC62C\uB9BC\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
숫자를 반올림한다는 것은 지정된 정밀도로 조정한다는 것을 의미합니다. 프로그래머들은 가독성을 위해 숫자를 단순화하거나, 특정 사양을 충족시키거나, 특정 범위 내에서 계산이 일치하도록 하기 위해 (예: 부동 소수점 산술에서의 정밀도 오류를 피하기 위해) 이 작업을 수행합니다.

## 방법:
Java는 숫자를 반올림하는 여러 방법을 제공합니다. 여기 `Math.round()`, `BigDecimal`, `DecimalFormat`을 사용한 간단한 데모가 있습니다.

```java
public class RoundingDemo {
    public static void main(String[] args) {
        double num = 123.4567;

        // Math.round() 사용하기
        long roundedNum = Math.round(num);
        System.out.println(roundedNum); // 출력: 123

        // 더 많은 제어를 위해 BigDecimal 사용하기
        BigDecimal bd = new BigDecimal(num).setScale(2, RoundingMode.HALF_UP);
        double roundedBigDecimal = bd.doubleValue();
        System.out.println(roundedBigDecimal); // 출력: 123.46

        // DecimalFormat 사용하기
        DecimalFormat df = new DecimalFormat("#.##");
        String formattedNum = df.format(num);
        System.out.println(formattedNum); // 출력: 123.46
    }
}
```

## 심층 탐구
역사적으로, 숫자를 반올림하는 것은 아날로그 계산에 필수적이었으며, 효율성과 정확성을 위해 디지털 컴퓨팅으로 전해져 왔습니다. 부동 소수점 산술에서 발생하는 반올림 오류와 같은 것들은 이것이 사소한 문제가 아님이 보여줍니다 - 그것들은 누적되어, 예를 들어, 항공우주 및 금융 애플리케이션에서 계산을 망칠 수 있습니다.

`Math.round()`를 넘어서서, `BigDecimal`은 규모와 반올림 모드에 대한 더 세밀한 제어를 제공하고, `DecimalFormat`은 텍스트 출력을 포맷하는 일부로 숫자를 반올림해야 할 때 사용됩니다. 반올림 방법 외에도 바닥함수, 천장함수, 절단 같은 다른 방법들이 있으며, 이는 일반적으로 다양한 `Math` 메소드에 의해 처리됩니다.

사용 사례에 따라, 반올림 전략이 달라질 수 있습니다. 예를 들어, `BigDecimal`은 정밀도가 매우 중요한 금융 계산에 있어서 가장 적합한 방법입니다. 반면에, `Math.round()`는 반올림 모드에 그다지 까다롭지 않은 일반적인 작업을 위한 빠른 방법입니다.

## 참고
- [오라클의 Java Math 문서](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/Math.html)
- [부동 소수점 산술을 위한 IEEE 표준 (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
- [Java의 DecimalFormat 클래스](https://docs.oracle.com/javase/7/docs/api/java/text/DecimalFormat.html)
