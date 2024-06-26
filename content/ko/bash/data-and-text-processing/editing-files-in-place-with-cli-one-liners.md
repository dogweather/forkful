---
date: 2024-01-27 16:21:42.084490-07:00
description: "\uBC29\uBC95: Bash\uB97C \uC0AC\uC6A9\uD558\uC5EC \uD30C\uC77C\uC744\
  \ \uC81C\uC790\uB9AC\uC5D0\uC11C \uD3B8\uC9D1\uD560 \uB54C, \uC8FC\uB85C `sed`\uC640\
  \ `awk`\uB77C\uB294 \uB450 \uAC00\uC9C0 \uC911\uC694\uD55C \uB3C4\uAD6C\uAC00 \uC0AC\
  \uC6A9\uB429\uB2C8\uB2E4. \uC774 \uAC15\uB825\uD55C \uC720\uD2F8\uB9AC\uD2F0\uB4E4\
  \uC744 \uC0AC\uC6A9\uD558\uB294 \uBC29\uBC95\uC744 \uBA87 \uAC00\uC9C0 \uCF54\uB529\
  \ \uC608\uC81C\uC640 \uD568\uAED8 \uC54C\uC544\uBD05\uC2DC\uB2E4."
lastmod: '2024-04-05T21:53:57.149585-06:00'
model: gpt-4-0125-preview
summary: "Bash\uB97C \uC0AC\uC6A9\uD558\uC5EC \uD30C\uC77C\uC744 \uC81C\uC790\uB9AC\
  \uC5D0\uC11C \uD3B8\uC9D1\uD560 \uB54C, \uC8FC\uB85C `sed`\uC640 `awk`\uB77C\uB294\
  \ \uB450 \uAC00\uC9C0 \uC911\uC694\uD55C \uB3C4\uAD6C\uAC00 \uC0AC\uC6A9\uB429\uB2C8\
  \uB2E4."
title: "CLI \uD55C \uC904 \uBA85\uB839\uC5B4\uB85C \uD30C\uC77C\uC744 \uC81C\uC790\
  \uB9AC\uC5D0\uC11C \uD3B8\uC9D1\uD558\uAE30"
weight: 32
---

## 방법:
Bash를 사용하여 파일을 제자리에서 편집할 때, 주로 `sed`와 `awk`라는 두 가지 중요한 도구가 사용됩니다. 이 강력한 유틸리티들을 사용하는 방법을 몇 가지 코딩 예제와 함께 알아봅시다.

### 단순 텍스트 대체를 위한 `sed` 사용
다음 명령은 `file.txt`에서 "text1"의 첫 번째 발생을 "text2"로 대체합니다:

```Bash
sed -i 's/text1/text2/' file.txt
```

모든 발생을 대체하려면 (전역 대체), 끝에 `g`를 추가합니다:

```Bash
sed -i 's/text1/text2/g' file.txt
```

여러 파일을 한 번에 수정하려면:

```Bash
sed -i 's/text1/text2/g' file1.txt file2.txt file3.txt
```

### 보다 복잡한 조작을 위한 `awk` 사용
`awk`는 특히 필드 기반 데이터를 처리하는 텍스트 처리에 유용한 프로그래밍 기능으로 빛나는 또 다른 도구입니다.

쉼표로 구분된 `data.csv`에서 각 줄의 두 번째 필드를 `newValue`로 변경:

```Bash
awk -i inplace -F, '{$2="newValue"; print $0}' OFS=, data.csv
```

### 뛰어들기 전에 백업하기
하나의 실용적인 조언: 제자리에서 편집하기 전에 항상 백업을 만드세요. `sed`는 백업을 생성하기 위해 `-i` 옵션 뒤에 접미사를 사용할 수 있게 해줍니다.

```Bash
sed -i.bak 's/text1/text2/g' file.txt
```

이 명령은 대체를 수행하기 전에 원본 `file.txt`의 백업으로 `file.txt.bak`을 생성합니다.

## 심층 분석
명령 줄에서 직접 파일을 편집하는 능력은 Unix 철학의 자연스러운 진전으로 나타났습니다: 사용자들이 가능한 한 적은 키 입력으로 데이터를 효율적으로 관리하고 조작할 수 있게 하는 것입니다. 그러나, 이러한 능력은 그것이 수반하는 경고들을 가집니다.

### 역사적 맥락
Unix 도구인 `sed`와 `awk`는 Unix 초기부터 있었으며, 특화된, 조합 가능한 명령어에 중점을 둔 그것의 툴킷 철학의 일부로 제작되었습니다. 그것이 Unix의 무기고에 포함된 것은 명령 줄 인터페이스가 지배하는 환경에서 효율적인 텍스트 처리를 위한 필요에 대한 응답이었습니다.

### 대안
`sed`와 `awk`는 강력하지만, 유일한 옵션은 아닙니다. 예를 들어, Perl과 Python은 비슷한 제자리에서 편집 기능을 허용하는 명령 줄 옵션(`-p` 및 `-i`로 각각)을 가집니다. 복잡한 작업에 대해 더 읽기 쉬운 문법을 제공합니다.

```Bash
perl -pi -e 's/text1/text2/g' file.txt
```

```Bash
python -c "import fileinput, sys; [sys.stdout.write(line.replace('text1', 'text2')) for line in fileinput.input(files='file.txt', inplace=True)]"
```

각 대안에는 강점이 있습니다: Perl의 한 줄짜리 기능이 엄청나고, Python의 문법이 Unix 텍스트 처리 도구에 깊이 정통하지 않은 사람들에게 접근하기 더 쉽다고 할 수 있습니다.

### 구현 세부사항
기술적인 의미에서 제자리 편집은 정말로 "제자리에서" 이루어지지 않습니다. `sed -i`와 `awk -i inplace` 모두 처리된 출력이 저장되기 전에 원본 파일을 대체하는 임시 파일을 생성합니다. 이 접근 방식은 프로세스가 중단될 경우 파일이 손상되지 않도록 보장합니다. 이것이 주로 리소스와 권한에 관한 함축이 있습니다: 대상 파일의 디렉토리에서 파일을 생성할 수 있는 권한과 임시 파일에 필요한 충분한 디스크 공간이 있어야 합니다.

강력하지만, 제자리 편집 명령은 주의해서 사용해야 합니다. 잘못 배치된 정규 표현식은 데이터 손실로 이어질 수 있으며, 백업의 중요성을 강조합니다. 잠재적인 함정에도 불구하고, 이 명령들을 마스터하면 명령 줄에서 직접 빠르고 효율적인 파일 수정을 수행할 수 있는 능력이 크게 향상되어, 복잡한 작업을 수행하기 위해 간단하고 강력한 도구를 활용하는 Unix 철학을 구현합니다.
