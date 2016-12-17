import csv
def readLines(fname):
    with open(fname) as f:
            content = f.readlines()
    return content

lines = readLines('XCorrelations.table')
lines = [x.strip().split(' ') for x in lines][1:]
for i, x in enumerate(lines):
    lines[i][1] = x[1].strip('"')
    lines[i][2] = x[2].strip('"')
    lines[i][3] = int(x[3])
    lines[i][4] = float(x[4])

nodes = {}

for x in lines:
    if (x[4] > 0):
        if (x[1] not in nodes):
            nodes[x[1]] = [x[2]]
        else:
            nodes[x[1]] += [x[2]]
        if (x[2] not in nodes):
            nodes[x[2]] = [x[1]]
        else:
            nodes[x[2]] += [x[1]]

def dfs(graph, start):
    visited, stack = set(), [start]
    while stack:
        vertex = stack.pop()
        if vertex not in visited:
            visited.add(vertex)
        if (vertex in graph):
            stack.extend(set(graph[vertex]) - visited)
    return visited


components = []
toCheck = set(nodes.keys())

while(len(toCheck) > 0):
    component = dfs(nodes, next(iter(toCheck)))
    components.append(component)
    toCheck -= component

with open('posClusters.csv', 'w') as f:
    writer = csv.writer(f)
    writer.writerows(components)
