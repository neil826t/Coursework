package spell;

public class Node implements INode{
    private INode[] nodes;
    private int count;

    public Node() {
        nodes = new INode[26];
        count = 0;
    }

    @Override
    public void incrementValue() {
        count++;
    }

    @Override
    public int getValue() {
        return count;
    }

    @Override
    public INode[] getChildren() {
        return nodes;
    }
}
