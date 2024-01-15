---
title:                "Att arbeta med yaml"
html_title:           "Bash: Att arbeta med yaml"
simple_title:         "Att arbeta med yaml"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## Varför

YAML står för "YAML Ain't Markup Language" och är ett enkelt och lättläst format för att strukturera data. Det är vanligtvis används för att konfigurera program och konfigurationsfiler.

## Hur man gör

Vi kommer att använda detta enkla YAML-exempel för att visa hur man arbetar med YAML-filer:

```
apiVersion: v1
kind: Pod
metadata:
  name: my-pod
spec:
  containers:
    - name: my-app
      image: my-app-image
```

För att bearbeta denna YAML-fil i Bash, behöver vi först installera ett verktyg som heter "yq". Detta verktyg tillåter oss att enkelt manipulera YAML-data i Bash.

Installera yq med hjälp av apt-get:

```Bash
sudo apt-get install yq
```

Låt oss nu gå igenom några exempel på hur du kan hantera YAML-data med Bash och yq:

### Läsa data från en YAML-fil

Använd detta enkla kommando för att läsa data från vår YAML-fil:

```Bash
yq read my-pod.yaml
```

Output:

```
--- 
apiVersion: v1
kind: Pod
metadata: 
  name: my-pod
spec: 
  containers: 
    - name: my-app
      image: my-app-image
```

### Lägga till data till en YAML-fil

Du kan enkelt lägga till data till en YAML-fil med hjälp av det här kommandot:

```Bash
yq write my-pod.yaml spec.containers[0].ports[+] app=8080
```

Detta kommer att lägga till en ny port med värdet "8080" under "containers" i vår YAML-fil:

```
--- 
apiVersion: v1
kind: Pod
metadata: 
  name: my-pod
spec: 
  containers: 
    - name: my-app
      image: my-app-image
      ports: 
        - port: 80
          app: 8080
```

### Uppdatera data i en YAML-fil

Du kan uppdatera befintliga data i en YAML-fil genom att ange sökvägen till den specifika datan du vill uppdatera och det nya värdet:

```Bash
yq write my-pod.yaml spec.containers[0].image=my-new-image
```

Detta kommer att uppdatera "image" till "my-new-image" i vår YAML-fil.

## Djupdykning

YAML är ett mycket kraftfullt och användarvänligt format för att strukturera data. Det finns många fler funktioner och användningsområden för YAML, vilket ger en flexibilitet i konfiguration av program och databehandling. Det är också populärt inom DevOps, som det används för att konfigurera infrastruktur och molnresurser. För att lära dig mer om YAML och dess användningsområden, rekommenderar vi att du tittar på länkarna nedan.

## Se även

- [yq installation instructions](https://mikefarah.github.io/yq/#installation)
- [YAML format specification](https://yaml.org/spec/)
- [Practical examples of using YAML in DevOps](https://www.redhat.com/sysadmin/yaml-devops-examples)