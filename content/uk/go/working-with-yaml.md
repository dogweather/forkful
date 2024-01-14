---
title:                "Go: Робота з Yaml"
simple_title:         "Робота з Yaml"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## Чому

Язык програмування Go стає все більш популярним серед розробників завдяки своїй ефективності та простоті використання. YAML, з іншого боку, є форматом для структурування даних із засадами, що легко читаються людьми. Поєднуючи ці два інструменти, ми можемо швидко та з легкістю працювати з конфігураційними файлами та налаштуваннями програм, що робить його необхідним для будь-якого Go розробника.

## Як

Для роботи з YAML у Go, нам спочатку потрібно підключити необхідний пакет у нашій програмі. Це може бути зроблено за допомогою наступної команди:

```Go
import "gopkg.in/yaml.v2"
```

Як тільки пакет підключений, ми можемо приступити до роботи з YAML. Давайте розглянемо приклад, де ми маємо конфігураційний файл у форматі YAML:

```Go
configuration:
    server:
        host: "example.com"
        port: 8080
    database:
        username: "user"
        password: "password"
```

Щоб прочитати цей файл у нашій програмі, нам потрібно створити структуру, що відповідає структурі даних у файлі. Наприклад:

```Go
type Config struct {
    Server struct {
        Host string `yaml:"host"`
        Port int `yaml:"port"`
    } `yaml:"server"`
    Database struct {
        Username string `yaml:"username"`
        Password string `yaml:"password"`
    } `yaml:"database"`
}
```

Тепер, ми можемо прочитати файл та отримати дані у вигляді структури:

```Go
func main() {
    // Читаємо файл у буфер
    file, err := ioutil.ReadFile("config.yaml")
    if err != nil {
        log.Fatal(err)
    }

    // Створюємо новий екземпляр структури
    config := Config{}

    // Розпаковуємо дані з файлу у структуру
    err = yaml.Unmarshal(file, &config)
    if err != nil {
        log.Fatal(err)
    }

    // Виводимо дані
    fmt.Println(config.Server.Host) // Виведе "example.com"
    fmt.Println(config.Database.Username) // Виведе "user"
}
```

## Глибоко пірнаємо

У цій статті ми розглянули простий спосіб роботи з YAML у Go. Але це далеко не всі можливості. Можна зберігати дані у вигляді масивів, карти, структур або навіть користуватися призначеними структурами для зчитування певного типу даних. Також можливе зберігання коментарів та зміна значень дані на льоту з допомогою пакету "gopkg.in/yaml.v2".

## Дивіться також

- [Офіційна документація по пакету YAML у Go](https://godoc.org/gopkg.in