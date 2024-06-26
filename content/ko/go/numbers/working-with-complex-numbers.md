---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:14:15.507195-07:00
description: "\uBC29\uBC95: Go\uC5D0\uC11C\uB294 \uB0B4\uC7A5\uB41C `complex`, `real`,\
  \ `imag` \uD568\uC218\uC640 \uD568\uAED8 `complex64` \uBC0F `complex128` \uD0C0\uC785\
  (\uAC01\uAC01 64\uBE44\uD2B8\uC640 128\uBE44\uD2B8 \uBCF5\uC18C\uC218\uB97C \uB098\
  \uD0C0\uB0C4)\uC744 \uC0AC\uC6A9\uD558\uC5EC \uBCF5\uC18C\uC218\uB97C \uCC98\uB9AC\
  \uD569\uB2C8\uB2E4. \uC5EC\uAE30 \uAC04\uB2E8\uD55C \uC2DC\uC791 \uAC00\uC774\uB4DC\
  \uAC00 \uC788\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.445390-06:00'
model: gpt-4-0125-preview
summary: "Go\uC5D0\uC11C\uB294 \uB0B4\uC7A5\uB41C `complex`, `real`, `imag` \uD568\
  \uC218\uC640 \uD568\uAED8 `complex64` \uBC0F `complex128` \uD0C0\uC785(\uAC01\uAC01\
  \ 64\uBE44\uD2B8\uC640 128\uBE44\uD2B8 \uBCF5\uC18C\uC218\uB97C \uB098\uD0C0\uB0C4\
  )\uC744 \uC0AC\uC6A9\uD558\uC5EC \uBCF5\uC18C\uC218\uB97C \uCC98\uB9AC\uD569\uB2C8\
  \uB2E4."
title: "\uBCF5\uC7A1\uD55C \uC22B\uC790\uB85C \uC791\uC5C5\uD558\uAE30"
weight: 14
---

## 방법:
Go에서는 내장된 `complex`, `real`, `imag` 함수와 함께 `complex64` 및 `complex128` 타입(각각 64비트와 128비트 복소수를 나타냄)을 사용하여 복소수를 처리합니다. 여기 간단한 시작 가이드가 있습니다:

```go
package main

import (
	"fmt"
)

func main() {
	// 복소수 생성
	a := complex(2, 3) // 2+3i
	b := complex(1, -1) // 1-1i

	// 산술 연산
	c := a + b
	fmt.Println("덧셈:", c) // 출력: 덧셈: (3+2i)

	d := a * b
	fmt.Println("곱셈:", d) // 출력: 곱셈: (5+1i)

	// 실수부와 허수부 접근
	realPart := real(a)
	imagPart := imag(a)
	fmt.Printf("실수부: %.1f, 허수부: %.1f\n", realPart, imagPart) // 출력: 실수부: 2.0, 허수부: 3.0

	// 복소수의 켤레와 크기는 계산할 수 있음
	conjugate := complex(real(a), -imag(a)) // 수동으로
	fmt.Println("a의 켤레:", conjugate) // 출력: a의 켤레: (2-3i)
}

```

이 예시는 기본을 다루지만, 크기, 위상 등을 찾는 데 사용할 수 있는 `math/cmplx` 패키지를 활용하여 복소수로 할 수 있는 것이 훨씬 더 많습니다.

## 심층 탐구
복소수 개념은 16세기로 거슬러 올라가지만, 광범위한 인정과 엄격한 형식화를 받은 것은 19세기였습니다. 컴퓨터 프로그래밍에서 복소수는 과학 및 공학 계산에서 복잡한 산술 연산에 필수적인 것으로, 초기부터 있어왔습니다. 프로그래밍 언어 중에서 Go가 복소수를 내장 지원과 `math/cmplx` 패키지를 통한 포괄적인 표준 라이브러리 지원으로 일급 시민으로 만든 접근 방식은 눈에 띕니다. 이러한 설계 결정은 Go의 단순성과 성능에 대한 강조를 반영합니다.

그럼에도 불구하고, Go에서 복소수를 다루는 것은 강력하지만 모든 애플리케이션에 항상 최선의 접근법은 아닐 수 있습니다. 특히 상징적 수학이나 고정밀도 산술을 요구하는 애플리케이션의 경우, NumPy와 SciPy와 같은 라이브러리를 가진 Python이나 MATLAB과 같은 소프트웨어는 특정 애플리케이션에 대해 더 많은 유연성과 폭넓은 기능 범위를 제공할 수 있습니다.

그럼에도 불구하고, 시스템 프로그래밍 및 복잡한 숫자 계산을 성능에 민감한 더 큰 애플리케이션에 통합하는 것이 중요한 맥락에서는, Go의 복소수에 대한 네이티브 지원은 독특하게 효율적인 옵션을 제공합니다.
