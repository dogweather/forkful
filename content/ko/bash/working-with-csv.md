---
title:                "Bash: CSV 작업하기"
simple_title:         "CSV 작업하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/working-with-csv.md"
---

{{< edit_this_page >}}

# 왜

CSV는 데이터를 저장하고 공유하는 데 매우 유용한 파일 형식입니다. Bash 프로그래밍을 사용하면 CSV 파일을 쉽게 작업할 수 있습니다. CSV 파일을 효율적으로 다루기 위해 Bash를 사용하는 이유를 알아보겠습니다.

# 어떻게

CSV 파일을 작성하려면 여러 가지 방법이 있지만 Bash 프로그래밍은 가장 간단하고 빠른 방법 중 하나입니다. 다음은 Bash를 사용하여 CSV 파일을 작성하는 예시입니다.

```Bash
# CSV 파일 작성하기
echo "name, age, country" > data.csv
echo "John, 25, USA" >> data.csv
echo "Jane, 30, Canada" >> data.csv

# CSV 파일 내용 출력하기
cat data.csv
```

위 코드를 실행하면 총 3줄의 데이터가 있는 CSV 파일이 작성되며, `cat data.csv`를 사용하면 다음과 같은 결과를 볼 수 있습니다.

```
name, age, country
John, 25, USA
Jane, 30, Canada
```

CSV 파일을 작성하는 이외에도 Bash를 사용하여 CSV 파일에서 데이터를 추출하거나 수정하는 등 다양한 작업을 할 수 있습니다.

# 깊이 있는 내용

Bash는 다음과 같은 명령어를 사용하여 CSV 파일에서 작업할 수 있도록 지원합니다.

- `awk`: CSV 파일에서 원하는 열이나 특정 조건을 가진 행을 추출합니다.
- `sed`: CSV 파일에서 데이터를 수정합니다.
- `cut`: CSV 파일에서 원하는 열만 추출합니다.
- `grep`: 특정 패턴을 가진 데이터를 찾습니다.

또한 Bash 프로그래밍을 할 때 변수 사용, 조건문, 반복문 등의 기본적인 개념을 활용하여 보다 복잡한 작업도 가능합니다.

# 참고 자료

- [Bash 팁: CSV 파일 다루기](https://www.cyberciti.biz/faq/bash-scripting-using-csv-file/)
- [BASH: Working With CSV Files](https://www.linuxjournal.com/content/bash-working-csv-files)
- [Manipulating CSV files with bash](https://opensource.com/article/19/10/bash-magic-csv)